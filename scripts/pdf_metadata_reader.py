#!/usr/bin/env python

from PyPDF2 import PdfReader
import sys

def extract_metadata_PyPDF2(pdf_path: str) -> dict:
    reader = PdfReader(pdf_path)
    metadata = reader.metadata
    # Convert metadata to a standard Python dictionary for easier handling
    metadata_dict = {key: metadata[key] for key in metadata}
    return metadata_dict

# Example usage
metadata = extract_metadata_PyPDF2(sys.argv[0])
print(metadata)
