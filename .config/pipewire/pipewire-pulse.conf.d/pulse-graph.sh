#!/bin/sh

# This is the script that creates virtual sinks and sources in a Pipewire using pipewire-pulse. This must be loaded from pipewire-pulse.conf.

pactl load-module module-virtual-sink sink_name=TelegramOutput
pactl load-module module-virtual-source source_name=TelegramIn

pactl load-module module-virtual-sink sink_name=DiscordOut
pactl load-module module-virtual-source source_name=DiscordIn

pactl load-module module-virtual-sink sink_name=ElementOut
pactl load-module module-virtual-source source_name=ElementIn

pactl load-module module-virtual-sink sink_name=BrowserOut
pactl load-module module-virtual-source source_name=BrowserOut

pactl load-module module-virtual-sink sink_name=MeetOut
pactl load-module module-virtual-source source_name=MeetIn

pactl load-module module-virtual-sink sink_name=MPDOut

pactl load-module module-virtual-sink sink_name=VLCOut

pactl load-module module-virtual-sink sink_name=MPVOut

pactl load-module module-virtual-source source_name=Recording
