#!/usr/bin/env python

import logging
import subprocess
import os
from pathlib import Path
from typing import List, Optional

from contextlib import contextmanager
from typing import Generator, Optional

logging.basicConfig(level=logging.INFO,
                    format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)

MOUNT_ROOT: Path = Path("/run/media/haxscramper")

D_MAIN: Path = MOUNT_ROOT / "hax_backup_main"
D_BAC1: Path = MOUNT_ROOT / "hax_backup_1"
D_BAC2: Path = MOUNT_ROOT / "hax_backup_2"


@contextmanager
def change_dir(target_dir: Path) -> Generator[None, None, None]:
    """Temporarily change the working directory."""
    current_dir: Path = Path.cwd()
    try:
        os.chdir(target_dir)
        yield
    finally:
        os.chdir(current_dir)


def run_cmd(cmd: List[str]) -> None:
    """Run a subprocess command with logging."""
    logger.info(f"Executing: {cmd}")
    subprocess.run(cmd, check=True)


def left_right_sync(left: Path, right: Path) -> None:
    logger.info(f"Syncing from {left} to {right}")
    assert left.exists(), left
    run_cmd([
        "unison",
        str(left),
        str(right), "-batch", "-debug", "verbose", "-auto", "-force",
        str(left)
    ])
    logger.info(f"Sync completed: {left} -> {right}")


def mirror1(subpath: str) -> None:
    logger.info(f"Mirroring {subpath} to backup 1")
    left_right_sync(D_MAIN / subpath, D_BAC1 / subpath)


def mirror2(subpath: str) -> None:
    logger.info(f"Mirroring {subpath} to backup 2")
    left_right_sync(D_MAIN / subpath, D_BAC2 / subpath)


logger.info("Backup 1 disk is mounted, synchronizing")
# Sync calibre
home_path: Path = Path.home()
logger.info("Starting calibre sync")
left_right_sync(
    home_path / "defaultdirs/documents/calibre",
    D_MAIN / "defaultdirs/archive/calibre_backup",
)

left_right_sync(
    home_path / "defaultdirs/input/grabber",
    D_MAIN / "defaultdirs/archive/input_grabber",
)

if D_BAC1.exists():
    # Mirror directories
    logger.info("Starting directory mirroring")
    mirror1("defaultdirs/images")
    mirror1("defaultdirs/documents")
    mirror1("defaultdirs/archive")
    logger.info("All sync operations completed")

if D_BAC2.exists():
    logger.info("Backup 2 directory exists")
    with change_dir(D_MAIN.joinpath("defaultdirs/videos")):
        run_cmd([Path(os.getcwd()).joinpath("fetch-playlist.sh")])

    mirror2("defaultdirs/videos")
