# Connections Companion

A small helper tool for the **New York Times game _Connections_**.

Connections Companion lets you enter the day’s words and **move them around into groups** before submitting your guesses in the official NYT game. Rearranging the words visually makes it easier to spot patterns, test ideas, and feel confident before locking anything in.

> This is **not** a solver — it’s a thinking aid.

---

## What It Does

- Enter or paste up to **16 words** (I usually use speech-to-text)
- Arrange them in a **4×4 grid**
- Drag and drop words to explore different groupings
- Works on desktop and mobile
- Use the color-coded rows to help visually separate potential groups

---

## Why This Exists

The Connections puzzle is all about pattern recognition — but the official game doesn’t let you freely rearrange the words.

This tool gives you a scratch space to:

- Try out groupings without committing
- Spot misleading overlaps
- Reduce second-guessing before submitting in the real game


---

## Getting Started

### Prerequisites

- [Elm](https://elm-lang.org/)

### Run Locally

1. Compile the Elm app:

    ```bash
    elm make src/Main.elm --output=main.js
    ```

2. Open `index.html` in your browser, or serve the directory with a simple static file server.

    For example, using Python:
    
    ```bash
    python3 -m http.server
    ```
    
    Then visit:
    
    ```
    http://localhost:8000
    ```

---

## ⚠️ Disclaimer

This project is **not affiliated with or endorsed by The New York Times**.  
Connections is a trademark of The New York Times Company.

This tool is for personal use and puzzle enjoyment only.

---

## Built With

- [Elm](https://elm-lang.org/)
- [`elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) for layout and styling

---

## Ideas for the Future

- Keyboard-based movement
- Optional labels for suspected categories
- List previous guesses for reference
- Entering words by uploading a screenshot and using OCR

---

Have fun, and good luck finding those connections!
