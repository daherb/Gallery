# Gallery

Simple Haskell script go generate HTML galleries

## Dependencies

It uses `base64` and `imagemagick`, both should be available on Linux. It also uses a few
basic Haskell modules.

## Usage

You can use `runghc Gallery.hs` in the directory containing your pictures. It generates a list
of all recognized files, creates thumbnails in a subdirectory and generates a `index.html` file.

Many parameters can be configured by changing the constants at the top.

The HTML file also by default includes a `gallery.css` file that can be used to customize the gallery.

## Disclaimer

Use at own risk
