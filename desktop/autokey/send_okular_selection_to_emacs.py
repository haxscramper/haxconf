#!/usr/bin/env python

import subprocess
import PyPDF2
import sys
from PySide6.QtCore import QCoreApplication
from PySide6.QtDBus import QDBusConnection, QDBusMessage


def get_current_selection():
    # Get the current selection using the xsel command-line tool
    selection = subprocess.check_output(["xsel", "-o", "-p"]).decode(
        "utf-8"
    )
    return selection.strip()


bus = QDBusConnection.sessionBus()


def get_current_pdf():
    services = bus.interface().registeredServiceNames()
    okular = None

    # Assuming there is one okular instance running, we need to get it.
    # Full name is going to be `org.kde.okular-115354`, which seems to
    # change dynamically.
    for name in services.value():
        if "okular" in name:
            okular = name
            break

    # Full list of APIs for D-Bus is provided in the introspection call,
    # use d-feet to debug.
    get_document = QDBusMessage.createMethodCall(
        name, "/okular", "org.kde.okular", "currentDocument"
    )

    get_page = QDBusMessage.createMethodCall(
        name, "/okular", "org.kde.okular", "currentPage"
    )

    document = bus.call(get_document)
    page = bus.call(get_page)
    return (document.arguments()[0], page.arguments()[0])


def send_selection_to_dbus(logical, physical, selection):
    # Get a proxy object for the destination interface, names are temporary
    # emacs debug solutions, will be changed later on.
    call = QDBusMessage.createMethodCall(
        "org.freedesktop.TextEditor",  # destination
        "/org/freedesktop/TextEditor",  # path
        "org.freedesktop.TextEditor",  # interface
        "OpenFile",  # method
    )

    # Can also use `setArguments` or some similar call, but `<<` seems ok
    call << logical
    call << physical
    call << selection

    # Call the SetSelection method on the interface with the selection string as an argument
    QDBusConnection.sessionBus().call(call)


def convert_page_numeration(num, format_str):
    # https://www.w3.org/TR/WCAG20-TECHS/PDF17.html
    #  /S specifies the numbering style for page numbers:
    #     /D - Arabic numerals (1,2,3...)
    #     /r - lowercase Roman numerals (i, ii, iii,...)
    #     /R - uppercase Roman numerals (I, II, III,...)
    #     /A - uppercase letters (A-Z)
    #     /a - lowercase letters (a-z)
    # /P (optional) - page number prefix
    # /St (optional) - the value of the first page number in the range (default: 1)

    # Code below written entirely in ChatGPT using specification
    # description above. The only thing it got wrong were lowercase roman
    # numerals, but I that's ~ok.
    if format_str == "/D":
        return str(num)
    elif format_str == "/r":
        roman_numerals = [
            "i",
            "iv",
            "v",
            "ix",
            "x",
            "xl",
            "l",
            "xc",
            "c",
            "cd",
            "d",
            "cm",
            "m",
        ]
        values = [1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000]
        result = ""
        i = 12
        while num > 0:
            div = num // values[i]
            num %= values[i]
            while div:
                result += roman_numerals[i]
                div -= 1
            i -= 1
        return result
    elif format_str == "/R":
        return format_number(num, "/r").upper()
    elif format_str == "/A":
        letters = ""
        while num > 0:
            num, remainder = divmod(num - 1, 26)
            letters = chr(65 + remainder) + letters
        return letters
    elif format_str == "/a":
        letters = ""
        while num > 0:
            num, remainder = divmod(num - 1, 26)
            letters = chr(97 + remainder) + letters
        return letters
    else:
        return str(num)


def logical_pdf_to_physical(pdf_file, logical_page_num):
    # Open the PDF file
    with open(pdf_file, "rb") as f:
        # Create a PdfFileReader object
        pdf_reader = PyPDF2.PdfFileReader(f)

        elements = []

        for index, page in enumerate(
            pdf_reader.trailer["/Root"]["/PageLabels"]["/Nums"]
        ):
            if type(page) == PyPDF2.generic.IndirectObject:
                elements[-1][1] = page.getObject()

            else:
                elements.append([page, None])

        last_element = None
        for index, page in enumerate(pdf_reader.pages):
            for (start, mapping) in elements:
                if start == index:
                    last_element = (index, mapping)

            if index == logical_page_num:
                return convert_page_numeration(
                    logical_page_num - last_element[0],
                    last_element[1]["/S"],
                )

        return str(logical_page_num)


selection = get_current_selection()
(opened_pdf, logical_page) = get_current_pdf()
physical_page = logical_pdf_to_physical(opened_pdf, logical_page)
send_selection_to_dbus(logical_page, physical_page, selection.replace("\n", " "))
