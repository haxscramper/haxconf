#Enter script code

from PyQt5.QtDBus import QDBusConnection, QDBusInterface
from PyQt5.QtCore import QCoreApplication



def send_call(call: str, args = []):
    # Set up the D-Bus connection and interface
    bus = QDBusConnection.sessionBus()
    interface = QDBusInterface("org.mpris.MediaPlayer2.strawberry", "/org/mpris/MediaPlayer2", "", bus)

    # Check if the interface is valid
    if not interface.isValid():
        print("Error: Failed to connect to the Strawberry D-Bus interface")
        sys.exit(1)

    # Call 'Next' method
    interface.call(call, *args)

    print(f"Sent '{call}' call to the running 'strawberry' application with arguments: {args}")
