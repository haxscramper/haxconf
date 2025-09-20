#!/usr/bin/env python3

import argparse
import os
from PIL import Image

def add_borders_to_image(input_path, target_width, target_height):
    """
    Add white borders to an image to achieve the target aspect ratio.
    """
    # Open the original image
    with Image.open(input_path) as img:
        original_width, original_height = img.size
        
        # Calculate target aspect ratio
        target_ratio = target_width / target_height
        original_ratio = original_width / original_height
        
        if abs(target_ratio - original_ratio) < 0.001:
            # Already correct aspect ratio
            new_img = img.copy()
        elif target_ratio > original_ratio:
            # Need to add horizontal borders
            new_width = int(original_height * target_ratio)
            new_height = original_height
            
            # Create new image with white background
            new_img = Image.new('RGB', (new_width, new_height), 'white')
            
            # Calculate position to center the original image
            x_offset = (new_width - original_width) // 2
            new_img.paste(img, (x_offset, 0))
        else:
            # Need to add vertical borders
            new_width = original_width
            new_height = int(original_width / target_ratio)
            
            # Create new image with white background
            new_img = Image.new('RGB', (new_width, new_height), 'white')
            
            # Calculate position to center the original image
            y_offset = (new_height - original_height) // 2
            new_img.paste(img, (0, y_offset))
        
        return new_img

def main():
    parser = argparse.ArgumentParser(description='Add white borders to image to achieve target aspect ratio')
    parser.add_argument('input_image', help='Path to input image')
    parser.add_argument('width', type=int, help='Target aspect ratio width')
    parser.add_argument('height', type=int, help='Target aspect ratio height')
    
    args = parser.parse_args()
    
    input_path = args.input_image
    target_width = args.width
    target_height = args.height
    
    # Validate input file
    if not os.path.exists(input_path):
        print(f"Error: Input file '{input_path}' does not exist")
        return
    
    # Check if file extension is supported
    supported_extensions = {'.jpg', '.jpeg', '.png', '.webp'}
    file_ext = os.path.splitext(input_path)[1].lower()
    if file_ext not in supported_extensions:
        print(f"Error: Unsupported file format '{file_ext}'. Supported formats: {', '.join(supported_extensions)}")
        return
    
    try:
        # Process the image
        new_img = add_borders_to_image(input_path, target_width, target_height)
        
        # Generate output filename
        base_name = os.path.splitext(input_path)[0]
        output_path = f"{base_name}_{target_width}x{target_height}.png"
        
        # Save the new image
        new_img.save(output_path, 'PNG')
        print(f"Successfully created: {output_path}")
        
    except Exception as e:
        print(f"Error processing image: {e}")

if __name__ == "__main__":
    main()
