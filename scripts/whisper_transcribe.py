#!/usr/bin/env python

import os
import click
import whisper
from rich.logging import RichHandler
import logging
import csv
from pathlib import Path
import itertools

# Set up rich logging
logging.basicConfig(level="INFO",
                    format="%(message)s",
                    datefmt="[%X]",
                    handlers=[RichHandler()])

logger = logging.getLogger("whisper-transcriber")


def transcribe_file(model, file_path, vtt_path, csv_path, language):
    logger.info(f"Transcribing: {file_path} using {model} model in {language}")
    logger.info(f"Writing vtt to {vtt_path}")

    model = whisper.load_model(model)
    result = model.transcribe(file_path, language=language, fp16=False, verbose=None)

    # Save to VTT
    with open(vtt_path, "w") as vtt_file:
        for segment in result['segments']:
            vtt_file.write(f"{segment['start']} --> {segment['end']}\n")
            vtt_file.write(f"{segment['text']}\n\n")

    # Save to CSV
    with open(csv_path, mode='w', newline='', encoding='utf-8') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(["start", "end", "text"])
        for segment in result["segments"]:
            writer.writerow([segment["start"], segment["end"], segment["text"]])

    logger.info(f"Transcription complete. VTT: {vtt_path}, CSV: {csv_path}")

@click.group()
def cli():
    """CLI tool for transcribing and processing audio files."""
    pass

@cli.command('transcribe')
@click.argument('file_path', type=click.Path(exists=True, dir_okay=False))
@click.option('--model', '-m', type=click.Choice(['tiny', 'base', 'small', 'medium', 'large'], case_sensitive=False), default='base', help='Whisper model to use for transcription.')
@click.option('--language', '-l', type=click.Choice(whisper.tokenizer.LANGUAGES, case_sensitive=False), default='english', help='Language to transcribe in.')
@click.option('--vtt-path', '-v', type=click.Path(), default=None, help='Custom path for VTT output. Defaults to input file path.')
@click.option('--csv-path', '-c', type=click.Path(), default=None, help='Custom path for CSV output. Defaults to input file path.')
def transcribe(file_path, model, language, vtt_path, csv_path):
    custom_vtt_path = vtt_path if vtt_path else Path(file_path).with_suffix(".vtt")
    custom_csv_path = csv_path if csv_path else Path(file_path).with_suffix(".csv")

    transcribe_file(model, file_path, custom_vtt_path, custom_csv_path, language)

@cli.command('filter-csv')
@click.argument('input_csv', type=click.Path(exists=True, file_okay=True, dir_okay=False))
@click.argument('output_csv', type=click.Path(file_okay=True, dir_okay=False))
@click.option('--min-length', '-m', default=1.0, help='Minimum length of speech segment in seconds.')
def filter_csv(input_csv, output_csv, min_length):
    with open(input_csv, mode='r', newline='', encoding='utf-8') as csv_file:
        reader = csv.reader(csv_file)
        headers = next(reader)

        with open(output_csv, mode='w', newline='', encoding='utf-8') as out_file:
            writer = csv.writer(out_file)
            writer.writerow(headers)

            accumulated_text = ""
            accumulated_start = None
            accumulated_end = None

            for row in reader:
                start, end, text = float(row[0]), float(row[1]), row[2]

                if accumulated_start is None:
                    accumulated_start = start

                segment_length = end - accumulated_start

                if segment_length < min_length:
                    accumulated_text += " " + text
                    accumulated_end = end
                else:
                    if accumulated_text:
                        writer.writerow([accumulated_start, accumulated_end, accumulated_text.strip()])
                        accumulated_text = ""
                    writer.writerow([start, end, text])
                    accumulated_start = None

            # Write any remaining accumulated text
            if accumulated_text:
                writer.writerow([accumulated_start, accumulated_end, accumulated_text.strip()])
    
    logger.info(f"Filtered CSV created at: {output_csv}")

@cli.command('extract-mp3')
@click.argument('files', nargs=-1, type=click.Path(exists=True))
def extract_mp3(files):
    for file in files:
        mp3_file = Path(file).with_suffix('.mp3')
        if mp3_file.exists():
            logger.info(f"Conversion for {file} already exists")
        else:
            subprocess.run(['ffmpeg', '-n', '-i', file, '-vn', '-ab', '192k', '-map', '0:a', str(mp3_file)])

@cli.command('transcribe-all')
@click.argument('files', nargs=-1, type=click.Path(exists=True))
def transcribe_all(files):
    for file in files:
        vtt_file = Path(file).with_suffix('.vtt')
        if vtt_file.exists():
            logger.info(f"{file} already converted to {vtt_file}")
        else:
            logger.info(f"{vtt_file} missing, converting {file}")
            transcribe(file, 'base', 'en', str(vtt_file), str(Path(file).with_suffix('.csv')))

if __name__ == '__main__':
    cli()

