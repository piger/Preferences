#!/bin/bash
# Weather plugin for bitbar
# Icons stolen from: https://raw.githubusercontent.com/matryer/bitbar-plugins/master/Weather/ForecastIO/weather.15m.py

set -e

# for jq
PATH="/usr/local/bin:$PATH"

FORECAST_FILE="/tmp/dark-sky-forecast.json"
DARK_SKY_TOKEN=$HOME/.dark-sky-token


get_location() {
    # returns "XX.XX,-YY.YYY"
    CoreLocationCLI -format '%latitude,%longitude'
}

get_forecast() {
    # $1 = location
    # $2 = token
    local location="$1"
    local token="$2"
    curl -sSL -o "$FORECAST_FILE" \
         "https://api.darksky.net/forecast/${token}/${location}?lang=en&units=si"
}

get_icon() {
    case "$1" in
        "clear-day")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAmVBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAjHWqVAAAAMnRSTlMAAQIDBAUGCAoMFxgZHR4hIiU0NTc4QEFFRlhZZmeGh4qLra6xtL/AwdDR1tnb3Pf4+RkSsW4AAACvSURBVHgBJczZWqJgAADQw7+AouOMU2lGtrgYQhny/g/X9+XtuTgUhGDdZ0IC01JpO1YSCsnrJUabnWz+PiGohwM58m/4jEiW94uP4drWd8coIJuN56bpxoVQBrmiPUN3Av24i9cnVaUZ0uZ5bbXd5Gtzg7Afe3DqoGup8u+zuKW1jCAe/9ftMJz+PCwlxK/vv4TEYZgJTN7msv2jEC8vkgJJNW6V8hSkQO5XQqDwAzg9DY/cb+9eAAAAAElFTkSuQmCC'
            ;;
        "clear-night")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAmVBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAjHWqVAAAAMnRSTlMAAQIDBAUGBxMVFycoKSosMDE/QEpLTFhkZ2prdHZ6ipagoayusLS5u7zGyeXs8vn6+7VdeBQAAACGSURBVHjabU+HDoJQDLxKVRwo4saNgnv1/z/Oiy9PEkOTjrtuVEugqrUSiosDjwnHu3wdekZQP9m1eNjIMbT7d4d+Y32wmBrZEI0mZraCAtTFXUQxtRSCL5GSENy2DrOlawnQswGn+aGvCO1nzJxfe7bL0ebQ8rBJdli2IL/Txbm/5wTV8gEi7AeTMvh8mQAAAABJRU5ErkJggg=='
            ;;
        "rain")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA5klEQVQoFQXBMSuFYRgA0PO870cGlMFgUEpEBiuKgUGMdhmU5AfIbvYHlNGghCIpLBaUWSmDQSbdgdW9j3MChDRkWbdH4+Z8OvGl6AAFu9Kvb+nPq5a0iQoNtqRtRZg3DPakRVSgZR8AVeDKMypMSdNohKqgC6vSLHDgS0UAoOLBPQVLLrR1SQAUXJikwY9BFI3URlEUTGgBG9IMoGoAC9I2BI6lS6fWwJhrL9IRggDrbr1JT26kD4dWQEAIwIpzZ3YARQDQaBQwAbpVoAI6OqDfuz53QhsoAAgM6DGCBAAAAqN6EQD/vPo8tMz6bZYAAAAASUVORK5CYII='
            ;;
        "snow")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA40lEQVQoFQXBsSqFYRgA4Of9vv+4AHWUIhORwYqiMIjZKoMSNyC72Q0oo0EZTh1ZZFGSsigpE0omncKK1/MECGnQsj43xsx6c+Jd8QcU7ErfPqQfj3rSJio02JK2FWHOMNiTFlGBnn0AVIEzt6gwKU2hEaqCFlalGeDAu4oAQMWVSwqWdPxqSQAUdEzQ4EsbRSP9oigKxvWADWkaUDWAeWkbAsdS16k1MOrcnXSEIMC6C0/Sua704tAKCAgBeJau7QCKAKCFfvceAC0VAKBgxBAIAACggAVLCAAACAz49KqNAvwDhLU7zZRMOIUAAAAASUVORK5CYII='
            ;;
        "sleet")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA6klEQVQoFQXBry+FYRgA0PO873cVqiCoxgQVG4FwN1E3wWaiZIImEzXJBGN2N6aYovjxBxjdJLuBZO7ncU6AkMZ0DXkwYd67Mx+KP6BgR/r2KQ286EsbqNBgU9pShAXjYFdaQgX69gFQBa49ocK0NINGqAo6WJHmgAMfKgIAFffuKFjW0+pIABT0TNHgyyiKRmpRFAWT+sC6NAuoGsCitAWBU+nKhVUw4dKzdIwgwJpbr9KjGymd6IIIBBKs2DDi3IxtrepPAjSqAHBoHAENYCB0tEL4NawFKCAwak+rNcCbHwAgMOYIBQDgH8ebRS21EV3JAAAAAElFTkSuQmCC'
            ;;
        "wind")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAflBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACCtoPsAAAAKXRSTlMAAQIDBAUOGx8kJjE7PkVHS05camtvdneHiY+su8DBw8rU1dzm8PH6/PnPEUAAAABvSURBVHgBlc3XDoJgDEDh0+HAvQc4FBHt+7+gI8T8XvJdnKRpk9KSKykB7G/uzjMSRj9ijTRrc2MftweYvxnAJKa9oNPcjw+zYSzOd+pTvtnmxzquI3bPy4BlUVZVWawyEJyEKvaJu4J+y++l0MYLmKUFUAQQSQkAAAAASUVORK5CYII='
            ;;
        "fog")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA7UlEQVQoFQXBzyqEURwA0HPv/SazYGGh7NiwUzOlFHv5s1BWnmAW8wBW3gHlGSyUPIBkQcNkY+ElhEgyDb7v5xyAAiCBpAAJJGHJptqGZR+OHaGogYwD4dnIQN+J8KSLAgXrQg9tQMeDsIJChXN3SEhaWuDGACWDSa9oq2SNxhROLaKmQl+YAwBcG6KQwNCPLXvOdG3bdyWsokBG24Uw9iKEd/fWUAAymDWj6FiQQQEAAAAgkVDUDu36VIQsNCpvdrxKFQK3Rr5lAFljDAkwb1otAULlUSNrIONS+BNCCL9CDxUJMKGtkQAh+dLAP69nSf8xfn8mAAAAAElFTkSuQmCC'
            ;;
        "cloudy")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA70lEQVQoFQXBLUuDURgA0HPvs1cFtVhFHAaTWMVgGgODySKCQQSTSZPFbrEMi8Hgb1gzCDOINruw5EdUGcJg23s9ByAECAFCAECAEGDaFAiAwKaebz/uPfoz0LOBIAsTR56MnDkVBo6dGHt2YCJgVXEOAOBC0QQ63hAqDSE0VDLeXQKfrlEBgAo3vmBXsYIMADIWFftZ26u+UAOAWvjwopWNVAhJApAkgRlDWop1QEiSAKwp2tBVbJs3C2DOgh1FF0juFGMjhzL2DNWKWwkSaGq7Uvz6VnRsWQIpIckmYFlL9qAPQq0kQJYxBgSKGv4BLfNBIGx3s8kAAAAASUVORK5CYII='
            ;;
        "partly-cloudy-day")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAABCUlEQVQoFQXBLUjcYRwA4Od9714F56ZYLAbbmAjC4YIoWIzaLNo8kAmbzW5WYTCDa5v25YnBE0SYTTHIYTEsCSIs+MFO/z+fpwYggaKyb0RLUSFlQBI+mNDBG73oWPZeANTRFCYxZBCfhHEAsoJVDZvuPdjSMI0CFAAbwmcrwiZAlvDWjrYTYQGsuDfk2O+sEuZdW3LgBlu+K0LNs3NndPsjHOoHs376569HXwG+qQyDog6KO6dGfUHhxjq6JZD1YNuREaFJ3ZMBhJBkL/5jDJem3MGaMIMu0IVFYRqQYFf4iD4N9Huyh7qaDPBLaLkV2kLbOyQgySo0zblwbsqVHzqyCoAEACAD8Aph71BRnuBbowAAAABJRU5ErkJggg=='
            ;;
        "partly-cloudy-night")
            echo 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA6UlEQVQoFQXBvyqFYRwA4Od93885AxLDySAMkpiUXAByA7LZbTJYnc4iF+HPxC1YLCYGYVJSslDUCYtTzvD9PE8CAJBl1GoAAKAAKEABAFlt0rY1XZ8SACRZE5tC17uwiwJkGTCkdggOhGUUCmiZsu9VWMW4N/cmkGHGnRB6Tl0IGzp+QYZhPx6t2NACbaHvBA1gT18CNCTsCJfIwJEHNBUJSYVjT0iQ3Zoz4g8JYQDPRhHAkjANGFAw6NsZKqD4cGXIrAXQFl6MIQMs+hJCOHcjdFTIQJLVmtb1zNvSdeAaWQ1AAQBUEgD/3uo/+JyzvikAAAAASUVORK5CYII='
            ;;
        *)
            echo ""
            ;;
    esac
}

# get the forecast
location="$(get_location)"
token="$(cat $DARK_SKY_TOKEN)"
get_forecast "$location" "$token"

# set the browser url for the contextual menu
browser_url="https://darksky.net/forecast/${location}?lang=en&units=si"

# print current summary in the bar
icon="$(get_icon "$(jq -r .currently.icon $FORECAST_FILE)")"
jq -r "\"\(.currently.summary), \(.currently.temperature) °C | templateImage=$icon\"" "$FORECAST_FILE"

echo "---"

# Extended summary
# note: m/s is "meters per second"
jq -r '"Feels like: \(.currently.apparentTemperature | tostring)°, Wind speed: \(.currently.windSpeed | tostring)m/s, Humidity: \(.currently.humidity * 100 | round | tostring)% | font=VictorMono-Bold"' "$FORECAST_FILE"

# Minutely forecast
minutely="$(jq -r .minutely.summary $FORECAST_FILE)"
icon="$(get_icon "$(jq -r .minutely.icon $FORECAST_FILE)")"
echo "Minute: $minutely | templateImage=$icon href=$browser_url"

# Hourly forecast
hourly="$(jq -r .hourly.summary $FORECAST_FILE)"
icon="$(get_icon "$(jq -r .hourly.icon $FORECAST_FILE)")"
echo "Hour: $hourly | templateImage=$icon href=$browser_url"
