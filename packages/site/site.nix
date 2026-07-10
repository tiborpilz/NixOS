{ pkgs, ... }:
pkgs.runCommand "nixos-config-site"
{
  nativeBuildInputs = [ pkgs.pandoc ];
  readme = ../../README.md;
  styles = ../../site/styles.css;
  screenshots = ../../screenshots/output;
} ''
  mkdir -p $out/assets/screenshots
  cp $styles $out/styles.css
  cp $screenshots/*.png $out/assets/screenshots/

  {
    cat $readme
    echo
    echo "## Screenshots"
    for f in $out/assets/screenshots/*.png; do
      name=$(basename "$f" .png)
      echo
      echo "![$name](assets/screenshots/$name.png)"
    done
  } > index.md

  pandoc index.md \
    --standalone \
    --metadata pagetitle="tiborpilz/NixOS" \
    --css styles.css \
    -o $out/index.html
''
