#!/usr/bin/env python

import os
from plumbum import local
import tempfile
from click import group, command, argument, option, Path
import click
from rich.console import Console
from pathlib import Path

console = Console()


def transcribe(input_file: Path, output_file: Path, lang="en"):
    ffmpeg = local['ffmpeg']
    whisper_cpp_path = Path(os.path.expanduser('~/software/whisper.cpp'))
    whisper_transcribe = local[str(whisper_cpp_path.joinpath("main"))]
    os.chdir(str(whisper_cpp_path.parent))

    mp3_file = input_file.with_suffix(".mp3")
    if not os.path.exists(mp3_file):
        with console.status(
                f"[bold green]Converting {input_file} to MP3...[/bold green]"):
            ffmpeg.run([
                '-n',
                '-i',
                str(input_file),
                '-vn',
                '-ab',
                '192k',
                '-map',
                '0:a',
                str(mp3_file),
            ])

    with tempfile.NamedTemporaryFile(suffix='.wav', delete=True) as temp_wav:
        temp_file = temp_wav.name
        if os.path.exists(temp_file):
            os.remove(temp_file)

        with console.status(f"[bold green]Transcribing {input_file} ...[/bold green]"):
            ffmpeg([
                '-i',
                str(input_file),
                '-acodec',
                'pcm_s16le',
                '-ac',
                '2',
                '-ar',
                '16000',
                str(temp_file),
            ])

            whisper_transcribe([
                '--print-progress',
                '--language',
                lang,
                '--output-file',
                str(output_file),
                '--output-vtt',
                '--output-csv',
                '--print-colors',
                str(temp_file),
            ])


@group()
def cli():
    """ Transcription tool using Whisper """
    pass


@command()
@argument('input_file', type=click.Path(exists=True, dir_okay=False))
@argument('output_file')
@option('--lang', default='en', help='Language for transcription.')
def transcribe_single(input_file, output_file, lang):
    """ Transcribe a single file """
    transcribe(Path(input_file).absolute(), Path(output_file).absolute(), lang)


@command()
@argument('files', nargs=-1, type=click.Path(exists=True, dir_okay=False))
def transcribe_batch(files):
    """ Transcribe multiple files """
    for input_file in [Path(f).absolute() for f in files]:
        output_file = input_file.with_suffix(".vtt")

        if os.path.exists(output_file):
            console.print(f"{input_file} already converted to {output_file}")
        else:
            console.print(f"{output_file} missing, converting {input_file}")
            transcribe(input_file, output_file)


cli.add_command(transcribe_single)
cli.add_command(transcribe_batch)

if __name__ == "__main__":
    cli()
