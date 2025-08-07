#!/usr/bin/env python

from beartype.typing import List, Optional, Union, Literal, Dict
from beartype import beartype
import time
import functools
from pathlib import Path
import random
import yaml
import logging
from pydantic import BaseModel, field_validator
from StreamDeck.DeviceManager import DeviceManager
from StreamDeck.ImageHelpers import PILHelper
from StreamDeck.Devices.StreamDeck import StreamDeck
from StreamDeck.Devices.StreamDeckNeo import StreamDeckNeo
from PIL import Image, ImageDraw, ImageFont
import subprocess
import pyautogui
import threading
from plumbum import local
import pyperclip

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
)

logger = logging.getLogger(__name__)


class ExecuteScriptAction(BaseModel):
    type: Literal["execute_script"]
    script: str


class EmulateShortcutAction(BaseModel):
    type: Literal["emulate_shortcut"]
    shortcut: str


class ChangePageAction(BaseModel):
    type: Literal["change_page"]
    direction: Literal["forward", "backward"]


class TypeTextAction(BaseModel):
    type: Literal["type_text"]
    text: str


class FocusWindowAction(BaseModel):
    type: Literal["focus_window"]
    text: str


class QuitAction(BaseModel):
    type: Literal["quit"]


Action = Union[
    ExecuteScriptAction,
    EmulateShortcutAction,
    ChangePageAction,
    TypeTextAction,
    FocusWindowAction,
    QuitAction,
]


class TextConfig(BaseModel):
    text: str
    font_size: int = 14
    color: str = "white"
    background_color: str = "black"


class ButtonConfig(BaseModel):
    action: Action
    image_path: Optional[Path] = None
    title_text: Optional[Union[str, TextConfig]] = None

    @field_validator("image_path")
    @classmethod
    def validate_image_path(cls, v: Optional[Path]) -> Optional[Path]:
        if v is None:
            return v

        expanded_path = Path(v).expanduser().resolve()

        if not expanded_path.exists():
            raise ValueError(f"Image file does not exist: {expanded_path}")

        return expanded_path


class PageConfig(BaseModel):
    name: str
    buttons: Dict[int, ButtonConfig]


class DeckConfig(BaseModel):
    device_type: str
    pages: List[PageConfig]


class Config(BaseModel):
    decks: List[DeckConfig]


@beartype
def focus_on_window(window_name: str):
    xprop = local["xprop"]
    wmctrl = local["wmctrl"]

    current_desktop = int(xprop("-root", "_NET_CURRENT_DESKTOP").split()[-1])
    base_tag = (current_desktop // 9) * 9

    windows = [
        it for it in wmctrl(("-l", "-x")).splitlines() if it.startswith("0x")
    ]
    logger.info(f"Current desktop: {current_desktop}")
    for w in windows:
        logger.info(w)

    def get_window_for_name(window_list):
        return next(
            (line.split()[0]
             for line in window_list if window_name.lower() in line.lower()),
            None)

    windows_on_current_tag = [
        line for line in windows
        if base_tag <= int(line.split()[1]) < base_tag + 9
    ]

    desired_window = get_window_for_name(windows_on_current_tag)

    if desired_window:
        logger.info(desired_window)
        wmctrl("-i", "-a", desired_window)

    else:
        logger.info(
            f"No specific window found, fallback to wmctrl first window")
        first_window = get_window_for_name(windows)
        if first_window:
            wmctrl("-i", "-a", first_window)


@beartype
class StreamDeckController:

    def __init__(self, config_path: Path):
        with open(config_path, "r") as f:
            config_data = yaml.safe_load(f)
        self.config = Config(**config_data)
        self.devices: Dict[str, StreamDeck] = {}
        self.current_pages = {}
        self.image_cache: Dict[str, Dict[int, bytes]] = {}
        self.quit_script = False
        self.last_activity_time = {}
        self.quit_script = False

    def initialize_devices(self) -> None:
        device_manager = DeviceManager()
        devices = device_manager.enumerate()
        import time

        for deck in devices:
            deck.open()
            deck.reset()
            time.sleep(1)
            deck_name = deck.deck_type()
            for deck_config in self.config.decks:
                if deck_config.device_type == deck_name:
                    logger.info(f"Configuring device {deck_name}")
                    self.devices[deck_name] = deck
                    self.current_pages[deck_name] = 0
                    deck.set_key_callback(lambda deck, key, state: self.
                                          key_callback(deck, key, state))
                    self.last_activity_time[deck_name] = time.time()
                    self.pregenerate_images(deck_name)
                    self.update_display(deck)
                    break

    def pregenerate_images(self, device_name: str) -> None:
        device = self.devices[device_name]
        deck_config = next(d for d in self.config.decks
                           if d.device_type == device_name)

        self.image_cache[device_name] = {}

        empty_image = self.create_empty_image(device)

        for page_index, page in enumerate(deck_config.pages):
            self.image_cache[device_name][page_index] = {}

            for key in range(device.key_count()):
                if key in page.buttons:
                    button = page.buttons[key]
                    image = self.create_button_image(device, button,
                                                     page_index, key)
                else:
                    image = empty_image

                self.image_cache[device_name][page_index][key] = image

    def set_random_touch_color(self, deck: StreamDeck, key: int):
        r = random.randint(0, 255)
        g = random.randint(0, 255)
        b = random.randint(0, 255)

        deck.set_key_color(key, r, g, b)

    def key_callback(self, deck: StreamDeck, key: int, state: bool) -> None:
        self.update_display(deck)
        self.last_activity_time[deck.deck_type] = time.time()
        if not state:
            return

        if key >= deck.key_count():
            self.set_random_touch_color(deck, key)

        device_name = deck.deck_type()
        logger.info(f"Triggering key {key} on device {device_name}")
        deck_config = next(d for d in self.config.decks
                           if d.device_type == device_name)
        current_page = self.current_pages[device_name]

        if current_page < len(deck_config.pages):
            page: PageConfig = deck_config.pages[current_page]
            key: ButtonConfig = page.buttons.get(key, None)
            if key:
                logger.info(f"Action on device {device_name} for key {key}")
                self.execute_action(deck, key.action)

    def execute_action(self, deck: StreamDeck, action: Action) -> None:
        match action:
            case ExecuteScriptAction():
                threading.Thread(target=lambda: subprocess.run(
                    action.script, shell=True)).start()
            case EmulateShortcutAction():
                threading.Thread(target=lambda: pyautogui.hotkey(
                    *action.shortcut.split("+"))).start()

            case ChangePageAction():
                self.change_page(deck, action.direction)

            case TypeTextAction():
                threading.Thread(
                    target=lambda: self.fast_type_text(action.text)).start()

            case QuitAction():
                self.quit_script = True

            case FocusWindowAction():
                threading.Thread(
                    target=lambda: focus_on_window(action.text)).start()

            case _:
                raise ValueError(f"Unknown action type {action}")

    def fast_type_text(self, text: str) -> None:
        import pyperclip

        original_clipboard = pyperclip.paste()
        pyperclip.copy(text)
        pyautogui.hotkey("ctrl", "v")
        pyperclip.copy(original_clipboard)

    def show_page_indicator(self, device_name: str, page_number: int) -> None:
        if not hasattr(self, "page_indicator_cache"):
            self.page_indicator_cache = {}

        cache_key = (device_name, page_number)

        if cache_key not in self.page_indicator_cache:
            device = self.devices[device_name]
            size = (device.screen_image_format()["size"][0],
                    device.screen_image_format()["size"][1])

            image = Image.new("RGB", size, "blue")
            draw = ImageDraw.Draw(image)

            try:
                font = ImageFont.truetype("arial.ttf", 32)
            except OSError:
                font = ImageFont.load_default(32)

            text = str(page_number)
            bbox = draw.textbbox((0, 0), text, font=font)
            text_width = bbox[2] - bbox[0]
            text_height = bbox[3] - bbox[1]

            x = (size[0] - text_width) // 2
            y = (size[1] - text_height) // 2

            draw.text((x, y), text, font=font, fill="white")

            native_image = PILHelper.to_native_format(device, image)
            self.page_indicator_cache[cache_key] = native_image

        device = self.devices[device_name]
        native_image = self.page_indicator_cache[cache_key]

        device.set_screen_image(native_image)

        import time
        time.sleep(0.5)

    def change_page(self, deck: StreamDeck, direction: str) -> None:
        deck_config = next(d for d in self.config.decks
                           if d.device_type == deck.deck_type())
        current_page = self.current_pages[deck.deck_type()]

        if direction == "forward":
            new_page = (current_page + 1) % len(deck_config.pages)
        else:
            new_page = (current_page - 1) % len(deck_config.pages)

        self.current_pages[deck.deck_type()] = new_page
        self.update_display(deck)

    def update_display(self, deck: StreamDeck) -> None:
        deck.set_brightness(100)
        current_page = self.current_pages[deck.deck_type()]

        for key in range(deck.key_count()):
            image = self.image_cache[deck.deck_type()][current_page][key]
            deck.set_key_image(key, image)

        if deck.is_visual():
            pass
            # self.show_page_indicator(deck.deck_type(), current_page)

    def create_button_image(self, device: StreamDeck, button: ButtonConfig,
                            page_number: int, button_number: int) -> bytes:
        if button.image_path and Path(button.image_path).exists():
            image = Image.open(button.image_path)
            if image.mode == "RGBA":
                background = Image.new("RGB", image.size, (0, 0, 0))
                background.paste(image, mask=image.split()[-1])
                image = background
            elif image.mode != "RGB":
                image = image.convert("RGB")
            image = image.resize((device.key_image_format()["size"][0],
                                  device.key_image_format()["size"][1]))
        elif button.title_text:
            image = self.create_text_image(device, button.title_text)
        else:
            image = self.create_default_action_image(device, page_number,
                                                     button_number)

        return PILHelper.to_native_format(device, image)

    def create_default_action_image(self, device: StreamDeck, page_number: int,
                                    button_number: int) -> Image.Image:
        size = (device.key_image_format()["size"][0],
                device.key_image_format()["size"][1])
        image = Image.new("RGB", size, "black")
        draw = ImageDraw.Draw(image)

        try:
            small_font = ImageFont.truetype("arial.ttf", 24)
            large_font = ImageFont.truetype("arial.ttf", 48)
        except OSError:
            small_font = ImageFont.load_default(24)
            large_font = ImageFont.load_default(48)

        page_text = str(page_number)
        button_text = str(button_number)

        page_bbox = draw.textbbox((0, 0), page_text, font=small_font)
        button_bbox = draw.textbbox((0, 0), button_text, font=large_font)

        page_width = page_bbox[2] - page_bbox[0]
        page_height = page_bbox[3] - page_bbox[1]
        button_width = button_bbox[2] - button_bbox[0]
        button_height = button_bbox[3] - button_bbox[1]

        total_height = page_height + button_height + 5

        page_x = (size[0] - page_width) // 2
        page_y = (size[1] - total_height) // 2

        button_x = (size[0] - button_width) // 2
        button_y = page_y + page_height + 5

        draw.text((page_x, page_y), page_text, font=small_font, fill="green")
        draw.text((button_x, button_y),
                  button_text,
                  font=large_font,
                  fill="red")

        return image

    def clear_all_keys(self, device_name: str) -> None:
        device = self.devices[device_name]
        empty_image = self.create_empty_image(device)

        for key in range(device.key_count()):
            device.set_key_image(key, empty_image)

    def create_text_image(self, device: StreamDeck,
                          title_text: Union[str, TextConfig]) -> Image.Image:
        size = (device.key_image_format()["size"][0],
                device.key_image_format()["size"][1])

        if isinstance(title_text, str):
            text = title_text
            font_size = 14
            color = "white"
            background_color = "black"
        else:
            text = title_text.text
            font_size = title_text.font_size
            color = title_text.color
            background_color = title_text.background_color

        image = Image.new("RGB", size, background_color)
        draw = ImageDraw.Draw(image)

        try:
            font = ImageFont.truetype("arial.ttf", font_size)
        except OSError:
            font = ImageFont.load_default(font_size)

        bbox = draw.textbbox((0, 0), text, font=font)
        text_width = bbox[2] - bbox[0]
        text_height = bbox[3] - bbox[1]

        x = (size[0] - text_width) // 2
        y = (size[1] - text_height) // 2

        draw.text((x, y), text, font=font, fill=color)

        return image

    def create_empty_image(self, device: StreamDeck) -> bytes:
        image = Image.new("RGB", (device.key_image_format()["size"][0],
                                  device.key_image_format()["size"][1]),
                          "black")
        return PILHelper.to_native_format(device, image)

    def run(self) -> None:
        import time

        self.initialize_devices()
        try:
            while not self.quit_script:
                current_time = time.time()
                for device_name in self.devices:
                    if 60 < (current_time -
                             self.last_activity_time[device_name]):
                        self.devices[device_name].set_brightness(0)
                        self.last_activity_time[device_name] = current_time

                time.sleep(1)

        finally:
            self.cleanup()

    def cleanup(self) -> None:
        device: StreamDeck
        import time
        for device in self.devices.values():
            device.reset()
            time.sleep(1)
            device.close()


if __name__ == "__main__":
    controller = StreamDeckController(Path("streamdeck_handler.yaml"))
    controller.run()
