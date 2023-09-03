# This file has been generated by node2nix 1.11.1. Do not edit!

{nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib, globalBuildInputs ? []}:

let
  sources = {
    "@babel/parser-7.22.14" = {
      name = "_at_babel_slash_parser";
      packageName = "@babel/parser";
      version = "7.22.14";
      src = fetchurl {
        url = "https://registry.npmjs.org/@babel/parser/-/parser-7.22.14.tgz";
        sha512 = "1KucTHgOvaw/LzCVrEOAyXkr9rQlp0A1HiHRYnSUE9dmb8PvPW7o5sscg+5169r54n3vGlbx6GevTE/Iw/P3AQ==";
      };
    };
    "@emmetio/abbreviation-2.3.3" = {
      name = "_at_emmetio_slash_abbreviation";
      packageName = "@emmetio/abbreviation";
      version = "2.3.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/@emmetio/abbreviation/-/abbreviation-2.3.3.tgz";
        sha512 = "mgv58UrU3rh4YgbE/TzgLQwJ3pFsHHhCLqY20aJq+9comytTXUDNGG/SMtSeMJdkpxgXSXunBGLD8Boka3JyVA==";
      };
    };
    "@emmetio/css-abbreviation-2.1.8" = {
      name = "_at_emmetio_slash_css-abbreviation";
      packageName = "@emmetio/css-abbreviation";
      version = "2.1.8";
      src = fetchurl {
        url = "https://registry.npmjs.org/@emmetio/css-abbreviation/-/css-abbreviation-2.1.8.tgz";
        sha512 = "s9yjhJ6saOO/uk1V74eifykk2CBYi01STTK3WlXWGOepyKa23ymJ053+DNQjpFcy1ingpaO7AxCcwLvHFY9tuw==";
      };
    };
    "@emmetio/scanner-1.0.4" = {
      name = "_at_emmetio_slash_scanner";
      packageName = "@emmetio/scanner";
      version = "1.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@emmetio/scanner/-/scanner-1.0.4.tgz";
        sha512 = "IqRuJtQff7YHHBk4G8YZ45uB9BaAGcwQeVzgj/zj8/UdOhtQpEIupUhSk8dys6spFIWVZVeK20CzGEnqR5SbqA==";
      };
    };
    "@johnsoncodehk/pug-beautify-0.2.2" = {
      name = "_at_johnsoncodehk_slash_pug-beautify";
      packageName = "@johnsoncodehk/pug-beautify";
      version = "0.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/@johnsoncodehk/pug-beautify/-/pug-beautify-0.2.2.tgz";
        sha512 = "qqNS/YD0Nck5wtQLCPHAfGVgWbbGafxSPjNh0ekYPFSNNqnDH2kamnduzYly8IiADmeVx/MfAE1njMEjVeHTMA==";
      };
    };
    "@jridgewell/sourcemap-codec-1.4.15" = {
      name = "_at_jridgewell_slash_sourcemap-codec";
      packageName = "@jridgewell/sourcemap-codec";
      version = "1.4.15";
      src = fetchurl {
        url = "https://registry.npmjs.org/@jridgewell/sourcemap-codec/-/sourcemap-codec-1.4.15.tgz";
        sha512 = "eF2rxCRulEKXHTRiDrDy6erMYWqNw4LPdQ8UQA4huuxaQsVeRPFl2oM8oDGxMFhJUWZf9McpLtJasDDZb/Bpeg==";
      };
    };
    "@types/node-17.0.45" = {
      name = "_at_types_slash_node";
      packageName = "@types/node";
      version = "17.0.45";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/node/-/node-17.0.45.tgz";
        sha512 = "w+tIMs3rq2afQdsPJlODhoUEKzFP1ayaoyl1CcnwtIlsVe7K7bA1NGm4s3PraqTLlXnbIN84zuBlxBWo1u9BLw==";
      };
    };
    "@volar-plugins/css-2.0.0" = {
      name = "_at_volar-plugins_slash_css";
      packageName = "@volar-plugins/css";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/css/-/css-2.0.0.tgz";
        sha512 = "ZAXdRK6n6T5fwC3Et4rOofLS9VH919/ayFHo5tXNCA7k2wkrVU2Uno408+024/irpVsFCkSxipycSQJBtTFh+A==";
      };
    };
    "@volar-plugins/emmet-2.0.0" = {
      name = "_at_volar-plugins_slash_emmet";
      packageName = "@volar-plugins/emmet";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/emmet/-/emmet-2.0.0.tgz";
        sha512 = "fbi+fDjixvipXt6qX71Bci66RGho/LnxeA9GfGsezdDWdQr9VjHs/3ewEpgi3fJkPeZqSawAkG+v2YQEWG4QMg==";
      };
    };
    "@volar-plugins/html-2.0.0" = {
      name = "_at_volar-plugins_slash_html";
      packageName = "@volar-plugins/html";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/html/-/html-2.0.0.tgz";
        sha512 = "X/DlOX/qJ54v8NzS80ZuD0OOU+Txx4GH0cUffhIsd9yn55mP/vc4nH+Lxo0Yz2I8SkjIiLM+DssumvAJSjH+og==";
      };
    };
    "@volar-plugins/json-2.0.0" = {
      name = "_at_volar-plugins_slash_json";
      packageName = "@volar-plugins/json";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/json/-/json-2.0.0.tgz";
        sha512 = "o56Ei2D2jVs5opSILGcE3wHVg0dGZEQ9A6xsx+klCDJ2/ZNADrByefga0z8LKdD2ZZiIpUpjzvsM/Oh9+hZOWw==";
      };
    };
    "@volar-plugins/pug-2.0.0" = {
      name = "_at_volar-plugins_slash_pug";
      packageName = "@volar-plugins/pug";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/pug/-/pug-2.0.0.tgz";
        sha512 = "BDz1n7EUa+J51cBxdhj8jVfB6KywB0BHt6IaMu/HV8VXFLNpDQCCUMEkEb5Wm7u9iV54hYk1x4cZawGruhHGZw==";
      };
    };
    "@volar-plugins/pug-beautify-2.0.0" = {
      name = "_at_volar-plugins_slash_pug-beautify";
      packageName = "@volar-plugins/pug-beautify";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/pug-beautify/-/pug-beautify-2.0.0.tgz";
        sha512 = "25r8HwObez/LoNrb0+kbyKDbhlEJe/WgjYeYMJ/OlKu+60BvSWyDTDtGmkV9DHh921zeAHcFTkHebf7oX1KbmA==";
      };
    };
    "@volar-plugins/typescript-2.0.0" = {
      name = "_at_volar-plugins_slash_typescript";
      packageName = "@volar-plugins/typescript";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/typescript/-/typescript-2.0.0.tgz";
        sha512 = "392e4KyMikiU9QkC/ii1K/HCs5Zcf8h6GA9BTpuuhxXeYk8LOnqDX8xLHTFVBuTORCXzrikZEzNzXHdL/4tqEA==";
      };
    };
    "@volar-plugins/typescript-twoslash-queries-2.0.0" = {
      name = "_at_volar-plugins_slash_typescript-twoslash-queries";
      packageName = "@volar-plugins/typescript-twoslash-queries";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar-plugins/typescript-twoslash-queries/-/typescript-twoslash-queries-2.0.0.tgz";
        sha512 = "NwqBBruD1DvVmFVyPinOuuMGqpSroVTnl1R1vOnhbKquButOj+0b2k43Gn1fz/Uqe9hijLCxMEtMIIcW38ny8w==";
      };
    };
    "@volar/language-core-1.4.1" = {
      name = "_at_volar_slash_language-core";
      packageName = "@volar/language-core";
      version = "1.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar/language-core/-/language-core-1.4.1.tgz";
        sha512 = "EIY+Swv+TjsWpxOxujjMf1ZXqOjg9MT2VMXZ+1dKva0wD8W0L6EtptFFcCJdBbcKmGMFkr57Qzz9VNMWhs3jXQ==";
      };
    };
    "@volar/language-server-1.4.1" = {
      name = "_at_volar_slash_language-server";
      packageName = "@volar/language-server";
      version = "1.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar/language-server/-/language-server-1.4.1.tgz";
        sha512 = "UxhiN205o8ZfTnMNhRPCtW+ncrBtqZMd+f08Xf99Je4WB+SYyv3VNnIZEQDXfaTXR6mLUgQ1mDwPsUOLKKGY8A==";
      };
    };
    "@volar/language-service-1.4.1" = {
      name = "_at_volar_slash_language-service";
      packageName = "@volar/language-service";
      version = "1.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar/language-service/-/language-service-1.4.1.tgz";
        sha512 = "F30uT+xk20ZYpxRwNW9xBEoErSqd9zNW7iuFwSIX9bYO/12RLjB2I+vgM/GdPZnzZ37imXa76ykwqTRXrafigQ==";
      };
    };
    "@volar/source-map-1.4.1" = {
      name = "_at_volar_slash_source-map";
      packageName = "@volar/source-map";
      version = "1.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar/source-map/-/source-map-1.4.1.tgz";
        sha512 = "bZ46ad72dsbzuOWPUtJjBXkzSQzzSejuR3CT81+GvTEI2E994D8JPXzM3tl98zyCNnjgs4OkRyliImL1dvJ5BA==";
      };
    };
    "@volar/vue-language-core-1.6.5" = {
      name = "_at_volar_slash_vue-language-core";
      packageName = "@volar/vue-language-core";
      version = "1.6.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar/vue-language-core/-/vue-language-core-1.6.5.tgz";
        sha512 = "IF2b6hW4QAxfsLd5mePmLgtkXzNi+YnH6ltCd80gb7+cbdpFMjM1I+w+nSg2kfBTyfu+W8useCZvW89kPTBpzg==";
      };
    };
    "@volar/vue-language-service-1.6.5" = {
      name = "_at_volar_slash_vue-language-service";
      packageName = "@volar/vue-language-service";
      version = "1.6.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/@volar/vue-language-service/-/vue-language-service-1.6.5.tgz";
        sha512 = "2uZPvDfo8Bspq9u+RaQhsdONFhu35HBS7/ZDXhhmhyMfcN327e1vnvAhHPDfOb8XNFg/Cj54rVKegANpKZJTOg==";
      };
    };
    "@vscode/emmet-helper-2.9.2" = {
      name = "_at_vscode_slash_emmet-helper";
      packageName = "@vscode/emmet-helper";
      version = "2.9.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vscode/emmet-helper/-/emmet-helper-2.9.2.tgz";
        sha512 = "MaGuyW+fa13q3aYsluKqclmh62Hgp0BpKIqS66fCxfOaBcVQ1OnMQxRRgQUYnCkxFISAQlkJ0qWWPyXjro1Qrg==";
      };
    };
    "@vscode/l10n-0.0.11" = {
      name = "_at_vscode_slash_l10n";
      packageName = "@vscode/l10n";
      version = "0.0.11";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vscode/l10n/-/l10n-0.0.11.tgz";
        sha512 = "ukOMWnCg1tCvT7WnDfsUKQOFDQGsyR5tNgRpwmqi+5/vzU3ghdDXzvIM4IOPdSb3OeSsBNvmSL8nxIVOqi2WXA==";
      };
    };
    "@vscode/l10n-0.0.16" = {
      name = "_at_vscode_slash_l10n";
      packageName = "@vscode/l10n";
      version = "0.0.16";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vscode/l10n/-/l10n-0.0.16.tgz";
        sha512 = "JT5CvrIYYCrmB+dCana8sUqJEcGB1ZDXNLMQ2+42bW995WmNoenijWMUdZfwmuQUTQcEVVIa2OecZzTYWUW9Cg==";
      };
    };
    "@vue/compiler-core-3.3.4" = {
      name = "_at_vue_slash_compiler-core";
      packageName = "@vue/compiler-core";
      version = "3.3.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vue/compiler-core/-/compiler-core-3.3.4.tgz";
        sha512 = "cquyDNvZ6jTbf/+x+AgM2Arrp6G4Dzbb0R64jiG804HRMfRiFXWI6kqUVqZ6ZR0bQhIoQjB4+2bhNtVwndW15g==";
      };
    };
    "@vue/compiler-dom-3.3.4" = {
      name = "_at_vue_slash_compiler-dom";
      packageName = "@vue/compiler-dom";
      version = "3.3.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vue/compiler-dom/-/compiler-dom-3.3.4.tgz";
        sha512 = "wyM+OjOVpuUukIq6p5+nwHYtj9cFroz9cwkfmP9O1nzH68BenTTv0u7/ndggT8cIQlnBeOo6sUT/gvHcIkLA5w==";
      };
    };
    "@vue/compiler-sfc-3.3.4" = {
      name = "_at_vue_slash_compiler-sfc";
      packageName = "@vue/compiler-sfc";
      version = "3.3.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vue/compiler-sfc/-/compiler-sfc-3.3.4.tgz";
        sha512 = "6y/d8uw+5TkCuzBkgLS0v3lSM3hJDntFEiUORM11pQ/hKvkhSKZrXW6i69UyXlJQisJxuUEJKAWEqWbWsLeNKQ==";
      };
    };
    "@vue/compiler-ssr-3.3.4" = {
      name = "_at_vue_slash_compiler-ssr";
      packageName = "@vue/compiler-ssr";
      version = "3.3.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vue/compiler-ssr/-/compiler-ssr-3.3.4.tgz";
        sha512 = "m0v6oKpup2nMSehwA6Uuu+j+wEwcy7QmwMkVNVfrV9P2qE5KshC6RwOCq8fjGS/Eak/uNb8AaWekfiXxbBB6gQ==";
      };
    };
    "@vue/reactivity-3.3.4" = {
      name = "_at_vue_slash_reactivity";
      packageName = "@vue/reactivity";
      version = "3.3.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vue/reactivity/-/reactivity-3.3.4.tgz";
        sha512 = "kLTDLwd0B1jG08NBF3R5rqULtv/f8x3rOFByTDz4J53ttIQEDmALqKqXY0J+XQeN0aV2FBxY8nJDf88yvOPAqQ==";
      };
    };
    "@vue/reactivity-transform-3.3.4" = {
      name = "_at_vue_slash_reactivity-transform";
      packageName = "@vue/reactivity-transform";
      version = "3.3.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vue/reactivity-transform/-/reactivity-transform-3.3.4.tgz";
        sha512 = "MXgwjako4nu5WFLAjpBnCj/ieqcjE2aJBINUNQzkZQfzIZA4xn+0fV1tIYBJvvva3N3OvKGofRLvQIwEQPpaXw==";
      };
    };
    "@vue/shared-3.3.4" = {
      name = "_at_vue_slash_shared";
      packageName = "@vue/shared";
      version = "3.3.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/@vue/shared/-/shared-3.3.4.tgz";
        sha512 = "7OjdcV8vQ74eiz1TZLzZP4JwqM5fA94K6yntPS5Z25r9HDuGNzaGdgvwKYq6S+MxwF0TFRwe50fIR/MYnakdkQ==";
      };
    };
    "acorn-7.4.1" = {
      name = "acorn";
      packageName = "acorn";
      version = "7.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/acorn/-/acorn-7.4.1.tgz";
        sha512 = "nQyp0o1/mNdbTO1PO6kHkwSrmgZ0MT/jCCpNiwbUjGoRN4dlBhqJtoQuCnEOKzgTVwg0ZWiCoQy6SxMebQVh8A==";
      };
    };
    "balanced-match-1.0.2" = {
      name = "balanced-match";
      packageName = "balanced-match";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.2.tgz";
        sha512 = "3oSeUO0TMV67hN1AmbXsK4yaqU7tjiHlbxRDZOpH0KW9+CeX4bRAaX0Anxt0tx2MrpRpWwQaPwIlISEJhYU5Pw==";
      };
    };
    "brace-expansion-2.0.1" = {
      name = "brace-expansion";
      packageName = "brace-expansion";
      version = "2.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-2.0.1.tgz";
        sha512 = "XnAIvQ8eM+kC6aULx6wuQiwVsnzsi9d3WxzV3FpWTGA19F621kwdbsAcFKXgKUHZWsy+mY6iL1sHTxWEFCytDA==";
      };
    };
    "call-bind-1.0.2" = {
      name = "call-bind";
      packageName = "call-bind";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/call-bind/-/call-bind-1.0.2.tgz";
        sha512 = "7O+FbCihrB5WGbFYesctwmTKae6rOiIzmz1icreWJ+0aA7LJfuqhEso2T9ncpcFtzMQtzXf2QGGueWJGTYsqrA==";
      };
    };
    "character-parser-2.2.0" = {
      name = "character-parser";
      packageName = "character-parser";
      version = "2.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/character-parser/-/character-parser-2.2.0.tgz";
        sha512 = "+UqJQjFEFaTAs3bNsF2j2kEN1baG/zghZbdqoYEDxGZtJo9LBzl1A+m0D4n3qKx8N2FNv8/Xp6yV9mQmBuptaw==";
      };
    };
    "de-indent-1.0.2" = {
      name = "de-indent";
      packageName = "de-indent";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/de-indent/-/de-indent-1.0.2.tgz";
        sha512 = "e/1zu3xH5MQryN2zdVaF0OrdNLUbvWxzMbi+iNA6Bky7l1RoP8a2fIbRocyHclXt/arDrrR6lL3TqFD9pMQTsg==";
      };
    };
    "emmet-2.4.6" = {
      name = "emmet";
      packageName = "emmet";
      version = "2.4.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/emmet/-/emmet-2.4.6.tgz";
        sha512 = "dJfbdY/hfeTyf/Ef7Y7ubLYzkBvPQ912wPaeVYpAxvFxkEBf/+hJu4H6vhAvFN6HlxqedlfVn2x1S44FfQ97pg==";
      };
    };
    "estree-walker-2.0.2" = {
      name = "estree-walker";
      packageName = "estree-walker";
      version = "2.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/estree-walker/-/estree-walker-2.0.2.tgz";
        sha512 = "Rfkk/Mp/DL7JVje3u18FxFujQlTNR2q6QfMSMB7AvCBx91NGj/ba3kCfza0f6dVDbw7YlRf/nDrn7pQrCCyQ/w==";
      };
    };
    "function-bind-1.1.1" = {
      name = "function-bind";
      packageName = "function-bind";
      version = "1.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/function-bind/-/function-bind-1.1.1.tgz";
        sha512 = "yIovAzMX49sF8Yl58fSCWJ5svSLuaibPxXQJFLmBObTuCr0Mf1KiPopGM9NiFjiYBCbfaa2Fh6breQ6ANVTI0A==";
      };
    };
    "get-intrinsic-1.2.1" = {
      name = "get-intrinsic";
      packageName = "get-intrinsic";
      version = "1.2.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/get-intrinsic/-/get-intrinsic-1.2.1.tgz";
        sha512 = "2DcsyfABl+gVHEfCOaTrWgyt+tb6MSEGmKq+kI5HwLbIYgjgmMcV8KQ41uaKz1xxUcn9tJtgFbQUEVcEbd0FYw==";
      };
    };
    "has-1.0.3" = {
      name = "has";
      packageName = "has";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/has/-/has-1.0.3.tgz";
        sha512 = "f2dvO0VU6Oej7RkWJGrehjbzMAjFp5/VKPp5tTpWIV4JHHZK1/BxbFRtf/siA2SWTe09caDmVtYYzWEIbBS4zw==";
      };
    };
    "has-proto-1.0.1" = {
      name = "has-proto";
      packageName = "has-proto";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/has-proto/-/has-proto-1.0.1.tgz";
        sha512 = "7qE+iP+O+bgF9clE5+UoBFzE65mlBiVj3tKCrlNQ0Ogwm0BjpT/gK4SlLYDMybDh5I3TCTKnPPa0oMG7JDYrhg==";
      };
    };
    "has-symbols-1.0.3" = {
      name = "has-symbols";
      packageName = "has-symbols";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/has-symbols/-/has-symbols-1.0.3.tgz";
        sha512 = "l3LCuF6MgDNwTDKkdYGEihYjt5pRPbEg46rtlmnSPlUbgmB8LOIrKJbYYFBSbnPaJexMKtiPO8hmeRjRz2Td+A==";
      };
    };
    "has-tostringtag-1.0.0" = {
      name = "has-tostringtag";
      packageName = "has-tostringtag";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/has-tostringtag/-/has-tostringtag-1.0.0.tgz";
        sha512 = "kFjcSNhnlGV1kyoGk7OXKSawH5JOb/LzUc5w9B02hOTO0dfFRjbHQKvg1d6cf3HbeUmtU9VbbV3qzZ2Teh97WQ==";
      };
    };
    "he-1.2.0" = {
      name = "he";
      packageName = "he";
      version = "1.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/he/-/he-1.2.0.tgz";
        sha512 = "F/1DnUGPopORZi0ni+CvrCgHQ5FyEAHRLSApuYWMmrbSwoN2Mn/7k+Gl38gJnR7yyDZk6WLXwiGod1JOWNDKGw==";
      };
    };
    "is-expression-4.0.0" = {
      name = "is-expression";
      packageName = "is-expression";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-expression/-/is-expression-4.0.0.tgz";
        sha512 = "zMIXX63sxzG3XrkHkrAPvm/OVZVSCPNkwMHU8oTX7/U3AL78I0QXCEICXUM13BIa8TYGZ68PiTKfQz3yaTNr4A==";
      };
    };
    "is-regex-1.1.4" = {
      name = "is-regex";
      packageName = "is-regex";
      version = "1.1.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-regex/-/is-regex-1.1.4.tgz";
        sha512 = "kvRdxDsxZjhzUX07ZnLydzS1TU/TJlTUHHY4YLL87e37oUA49DfkLqgy+VjFocowy29cKvcSiu+kIv728jTTVg==";
      };
    };
    "jsonc-parser-2.3.1" = {
      name = "jsonc-parser";
      packageName = "jsonc-parser";
      version = "2.3.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/jsonc-parser/-/jsonc-parser-2.3.1.tgz";
        sha512 = "H8jvkz1O50L3dMZCsLqiuB2tA7muqbSg1AtGEkN0leAqGjsUzDJir3Zwr02BhqdcITPg3ei3mZ+HjMocAknhhg==";
      };
    };
    "jsonc-parser-3.2.0" = {
      name = "jsonc-parser";
      packageName = "jsonc-parser";
      version = "3.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/jsonc-parser/-/jsonc-parser-3.2.0.tgz";
        sha512 = "gfFQZrcTc8CnKXp6Y4/CBT3fTc0OVuDofpre4aEeEpSBPV5X5v4+Vmx+8snU7RLPrNHPKSgLxGo9YuQzz20o+w==";
      };
    };
    "lru-cache-6.0.0" = {
      name = "lru-cache";
      packageName = "lru-cache";
      version = "6.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/lru-cache/-/lru-cache-6.0.0.tgz";
        sha512 = "Jo6dJ04CmSjuznwJSS3pUeWmd/H0ffTlkXXgwZi+eq1UCmqQwCh+eLsYOYCwY991i2Fah4h1BEMCx4qThGbsiA==";
      };
    };
    "magic-string-0.30.3" = {
      name = "magic-string";
      packageName = "magic-string";
      version = "0.30.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/magic-string/-/magic-string-0.30.3.tgz";
        sha512 = "B7xGbll2fG/VjP+SWg4sX3JynwIU0mjoTc6MPpKNuIvftk6u6vqhDnk1R80b8C2GBR6ywqy+1DcKBrevBg+bmw==";
      };
    };
    "minimatch-9.0.3" = {
      name = "minimatch";
      packageName = "minimatch";
      version = "9.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/minimatch/-/minimatch-9.0.3.tgz";
        sha512 = "RHiac9mvaRw0x3AYRgDC1CxAP7HTcNrrECeA8YYJeWnpo+2Q5CegtZjaotWTWxDG3UeGA1coE05iH1mPjT/2mg==";
      };
    };
    "muggle-string-0.2.2" = {
      name = "muggle-string";
      packageName = "muggle-string";
      version = "0.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/muggle-string/-/muggle-string-0.2.2.tgz";
        sha512 = "YVE1mIJ4VpUMqZObFndk9CJu6DBJR/GB13p3tXuNbwD4XExaI5EOuRl6BHeIDxIqXZVxSfAC+y6U1Z/IxCfKUg==";
      };
    };
    "nanoid-3.3.6" = {
      name = "nanoid";
      packageName = "nanoid";
      version = "3.3.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/nanoid/-/nanoid-3.3.6.tgz";
        sha512 = "BGcqMMJuToF7i1rt+2PWSNVnWIkGCU78jBG3RxO/bZlnZPK2Cmi2QaffxGO/2RvWi9sL+FAiRiXMgsyxQ1DIDA==";
      };
    };
    "object-assign-4.1.1" = {
      name = "object-assign";
      packageName = "object-assign";
      version = "4.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz";
        sha512 = "rJgTQnkUnH1sFw8yT6VSU3zD3sWmu6sZhIseY8VX+GRu3P6F7Fu+JNDoXfklElbLJSnc3FUQHVe4cU5hj+BcUg==";
      };
    };
    "picocolors-1.0.0" = {
      name = "picocolors";
      packageName = "picocolors";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/picocolors/-/picocolors-1.0.0.tgz";
        sha512 = "1fygroTLlHu66zi26VoTDv8yRgm0Fccecssto+MhsZ0D/DGW2sm8E8AjW7NU5VVTRt5GxbeZ5qBuJr+HyLYkjQ==";
      };
    };
    "postcss-8.4.29" = {
      name = "postcss";
      packageName = "postcss";
      version = "8.4.29";
      src = fetchurl {
        url = "https://registry.npmjs.org/postcss/-/postcss-8.4.29.tgz";
        sha512 = "cbI+jaqIeu/VGqXEarWkRCCffhjgXc0qjBtXpqJhTBohMUjUQnbBr0xqX3vEKudc4iviTewcJo5ajcec5+wdJw==";
      };
    };
    "pug-error-2.0.0" = {
      name = "pug-error";
      packageName = "pug-error";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/pug-error/-/pug-error-2.0.0.tgz";
        sha512 = "sjiUsi9M4RAGHktC1drQfCr5C5eriu24Lfbt4s+7SykztEOwVZtbFk1RRq0tzLxcMxMYTBR+zMQaG07J/btayQ==";
      };
    };
    "pug-lexer-5.0.1" = {
      name = "pug-lexer";
      packageName = "pug-lexer";
      version = "5.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/pug-lexer/-/pug-lexer-5.0.1.tgz";
        sha512 = "0I6C62+keXlZPZkOJeVam9aBLVP2EnbeDw3An+k0/QlqdwH6rv8284nko14Na7c0TtqtogfWXcRoFE4O4Ff20w==";
      };
    };
    "pug-parser-6.0.0" = {
      name = "pug-parser";
      packageName = "pug-parser";
      version = "6.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/pug-parser/-/pug-parser-6.0.0.tgz";
        sha512 = "ukiYM/9cH6Cml+AOl5kETtM9NR3WulyVP2y4HOU45DyMim1IeP/OOiyEWRr6qk5I5klpsBnbuHpwKmTx6WURnw==";
      };
    };
    "request-light-0.7.0" = {
      name = "request-light";
      packageName = "request-light";
      version = "0.7.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/request-light/-/request-light-0.7.0.tgz";
        sha512 = "lMbBMrDoxgsyO+yB3sDcrDuX85yYt7sS8BfQd11jtbW/z5ZWgLZRcEGLsLoYw7I0WSUGQBs8CC8ScIxkTX1+6Q==";
      };
    };
    "semver-7.5.4" = {
      name = "semver";
      packageName = "semver";
      version = "7.5.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/semver/-/semver-7.5.4.tgz";
        sha512 = "1bCSESV6Pv+i21Hvpxp3Dx+pSD8lIPt8uVjRrxAUt/nbswYc+tK6Y2btiULjd4+fnq15PX+nqQDC7Oft7WkwcA==";
      };
    };
    "source-map-js-1.0.2" = {
      name = "source-map-js";
      packageName = "source-map-js";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/source-map-js/-/source-map-js-1.0.2.tgz";
        sha512 = "R0XvVJ9WusLiqTCEiGCmICCMplcCkIwwR11mOSD9CR5u+IXYdiseeEuXCVAjS54zqwkLcPNnmU4OeJ6tUrWhDw==";
      };
    };
    "token-stream-1.0.0" = {
      name = "token-stream";
      packageName = "token-stream";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/token-stream/-/token-stream-1.0.0.tgz";
        sha512 = "VSsyNPPW74RpHwR8Fc21uubwHY7wMDeJLys2IX5zJNih+OnAnaifKHo+1LHT7DAdloQ7apeaaWg8l7qnf/TnEg==";
      };
    };
    "typesafe-path-0.2.2" = {
      name = "typesafe-path";
      packageName = "typesafe-path";
      version = "0.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/typesafe-path/-/typesafe-path-0.2.2.tgz";
        sha512 = "OJabfkAg1WLZSqJAJ0Z6Sdt3utnbzr/jh+NAHoyWHJe8CMSy79Gm085094M9nvTPy22KzTVn5Zq5mbapCI/hPA==";
      };
    };
    "typescript-4.9.5" = {
      name = "typescript";
      packageName = "typescript";
      version = "4.9.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/typescript/-/typescript-4.9.5.tgz";
        sha512 = "1FXk9E2Hm+QzZQ7z+McJiHL4NW1F2EzMu9Nq9i3zAaGqibafqYwCVU6WyWAuyQRRzOlxou8xZSyXLEN8oKj24g==";
      };
    };
    "typescript-5.2.2" = {
      name = "typescript";
      packageName = "typescript";
      version = "5.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/typescript/-/typescript-5.2.2.tgz";
        sha512 = "mI4WrpHsbCIcwT9cF4FZvr80QUeKvsUsUvKDoR+X/7XHQH98xYD8YHZg7ANtz2GtZt/CBq2QJ0thkGJMHfqc1w==";
      };
    };
    "typescript-auto-import-cache-0.2.1" = {
      name = "typescript-auto-import-cache";
      packageName = "typescript-auto-import-cache";
      version = "0.2.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/typescript-auto-import-cache/-/typescript-auto-import-cache-0.2.1.tgz";
        sha512 = "FD5uYQSNkVTX4b3lvtifP+SR3bARWGmKe/uyp5BfuW2ZUCYG7vHKPddrteLU06Uh68woRaYIX+Sbs2nnySpGLw==";
      };
    };
    "vscode-css-languageservice-6.2.7" = {
      name = "vscode-css-languageservice";
      packageName = "vscode-css-languageservice";
      version = "6.2.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-css-languageservice/-/vscode-css-languageservice-6.2.7.tgz";
        sha512 = "Jd8wpIg5kJ15CfrieoEPvu3gGFc36sbM3qXCtjVq5zrnLEX5NhHxikMDtf8AgQsYklXiDqiZLKoBnzkJtRbTHQ==";
      };
    };
    "vscode-html-languageservice-5.0.7" = {
      name = "vscode-html-languageservice";
      packageName = "vscode-html-languageservice";
      version = "5.0.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-html-languageservice/-/vscode-html-languageservice-5.0.7.tgz";
        sha512 = "jX+7/kUXrdOaRT8vqYR/jLxrGDib+Far8I7n/A6apuEl88k+mhIHZPwc6ezuLeiCKUCaLG4b0dqFwjVa7QL3/w==";
      };
    };
    "vscode-json-languageservice-5.3.6" = {
      name = "vscode-json-languageservice";
      packageName = "vscode-json-languageservice";
      version = "5.3.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-json-languageservice/-/vscode-json-languageservice-5.3.6.tgz";
        sha512 = "P4kthBi3GMLKi7Lmp24nkKHAWxbFfCsIDBPlMrK1Tag1aqbl3l60UferDkfAasupDVBM2dekbArzGycUjw8OHA==";
      };
    };
    "vscode-jsonrpc-6.0.0" = {
      name = "vscode-jsonrpc";
      packageName = "vscode-jsonrpc";
      version = "6.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-6.0.0.tgz";
        sha512 = "wnJA4BnEjOSyFMvjZdpiOwhSq9uDoK8e/kpRJDTaMYzwlkrhG1fwDIZI94CLsLzlCK5cIbMMtFlJlfR57Lavmg==";
      };
    };
    "vscode-jsonrpc-8.1.0" = {
      name = "vscode-jsonrpc";
      packageName = "vscode-jsonrpc";
      version = "8.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-8.1.0.tgz";
        sha512 = "6TDy/abTQk+zDGYazgbIPc+4JoXdwC8NHU9Pbn4UJP1fehUyZmM4RHp5IthX7A6L5KS30PRui+j+tbbMMMafdw==";
      };
    };
    "vscode-languageserver-7.0.0" = {
      name = "vscode-languageserver";
      packageName = "vscode-languageserver";
      version = "7.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver/-/vscode-languageserver-7.0.0.tgz";
        sha512 = "60HTx5ID+fLRcgdHfmz0LDZAXYEV68fzwG0JWwEPBode9NuMYTIxuYXPg4ngO8i8+Ou0lM7y6GzaYWbiDL0drw==";
      };
    };
    "vscode-languageserver-8.1.0" = {
      name = "vscode-languageserver";
      packageName = "vscode-languageserver";
      version = "8.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver/-/vscode-languageserver-8.1.0.tgz";
        sha512 = "eUt8f1z2N2IEUDBsKaNapkz7jl5QpskN2Y0G01T/ItMxBxw1fJwvtySGB9QMecatne8jFIWJGWI61dWjyTLQsw==";
      };
    };
    "vscode-languageserver-protocol-3.16.0" = {
      name = "vscode-languageserver-protocol";
      packageName = "vscode-languageserver-protocol";
      version = "3.16.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.16.0.tgz";
        sha512 = "sdeUoAawceQdgIfTI+sdcwkiK2KU+2cbEYA0agzM2uqaUy2UpnnGHtWTHVEtS0ES4zHU0eMFRGN+oQgDxlD66A==";
      };
    };
    "vscode-languageserver-protocol-3.17.3" = {
      name = "vscode-languageserver-protocol";
      packageName = "vscode-languageserver-protocol";
      version = "3.17.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.17.3.tgz";
        sha512 = "924/h0AqsMtA5yK22GgMtCYiMdCOtWTSGgUOkgEDX+wk2b0x4sAfLiO4NxBxqbiVtz7K7/1/RgVrVI0NClZwqA==";
      };
    };
    "vscode-languageserver-textdocument-1.0.10" = {
      name = "vscode-languageserver-textdocument";
      packageName = "vscode-languageserver-textdocument";
      version = "1.0.10";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver-textdocument/-/vscode-languageserver-textdocument-1.0.10.tgz";
        sha512 = "dPA6WqtAQJ/Iopm0Hrj11VvaKxsEcm62jpqyaYbY0xuvUffeWAn77f3VKr2SCsJphSyEw4Fjkjqm2gQ24KQfrA==";
      };
    };
    "vscode-languageserver-types-3.16.0" = {
      name = "vscode-languageserver-types";
      packageName = "vscode-languageserver-types";
      version = "3.16.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.16.0.tgz";
        sha512 = "k8luDIWJWyenLc5ToFQQMaSrqCHiLwyKPHKPQZ5zz21vM+vIVUSvsRpcbiECH4WR88K2XZqc4ScRcZ7nk/jbeA==";
      };
    };
    "vscode-languageserver-types-3.17.3" = {
      name = "vscode-languageserver-types";
      packageName = "vscode-languageserver-types";
      version = "3.17.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.17.3.tgz";
        sha512 = "SYU4z1dL0PyIMd4Vj8YOqFvHu7Hz/enbWtpfnVbJHU4Nd1YNYx8u0ennumc6h48GQNeOLxmwySmnADouT/AuZA==";
      };
    };
    "vscode-nls-5.2.0" = {
      name = "vscode-nls";
      packageName = "vscode-nls";
      version = "5.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-nls/-/vscode-nls-5.2.0.tgz";
        sha512 = "RAaHx7B14ZU04EU31pT+rKz2/zSl7xMsfIZuo8pd+KZO6PXtQmpevpq3vxvWNcrGbdmhM/rr5Uw5Mz+NBfhVng==";
      };
    };
    "vscode-uri-2.1.2" = {
      name = "vscode-uri";
      packageName = "vscode-uri";
      version = "2.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-uri/-/vscode-uri-2.1.2.tgz";
        sha512 = "8TEXQxlldWAuIODdukIb+TR5s+9Ds40eSJrw+1iDDA9IFORPjMELarNQE3myz5XIkWWpdprmJjm1/SxMlWOC8A==";
      };
    };
    "vscode-uri-3.0.7" = {
      name = "vscode-uri";
      packageName = "vscode-uri";
      version = "3.0.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/vscode-uri/-/vscode-uri-3.0.7.tgz";
        sha512 = "eOpPHogvorZRobNqJGhapa0JdwaxpjVvyBp0QIUMRMSf8ZAlqOdEquKuRmw9Qwu0qXtJIWqFtMkmvJjUZmMjVA==";
      };
    };
    "vue-component-meta-1.6.5" = {
      name = "vue-component-meta";
      packageName = "vue-component-meta";
      version = "1.6.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/vue-component-meta/-/vue-component-meta-1.6.5.tgz";
        sha512 = "pps5DTFz9JiqidXMrPCf/Qt8sngQYzkU73CB4zLh5dsPlAMSm742+/zOR4MgKxuaZSPAwDFazSRhRCkpkywmCw==";
      };
    };
    "vue-component-type-helpers-1.6.5" = {
      name = "vue-component-type-helpers";
      packageName = "vue-component-type-helpers";
      version = "1.6.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/vue-component-type-helpers/-/vue-component-type-helpers-1.6.5.tgz";
        sha512 = "iGdlqtajmiqed8ptURKPJ/Olz0/mwripVZszg6tygfZSIL9kYFPJTNY6+Q6OjWGznl2L06vxG5HvNvAnWrnzbg==";
      };
    };
    "vue-template-compiler-2.7.14" = {
      name = "vue-template-compiler";
      packageName = "vue-template-compiler";
      version = "2.7.14";
      src = fetchurl {
        url = "https://registry.npmjs.org/vue-template-compiler/-/vue-template-compiler-2.7.14.tgz";
        sha512 = "zyA5Y3ArvVG0NacJDkkzJuPQDF8RFeRlzV2vLeSnhSpieO6LK2OVbdLPi5MPPs09Ii+gMO8nY4S3iKQxBxDmWQ==";
      };
    };
    "yallist-4.0.0" = {
      name = "yallist";
      packageName = "yallist";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/yallist/-/yallist-4.0.0.tgz";
        sha512 = "3wdGidZyq5PB084XLES5TpOSRA3wjXAlIWMhum2kRcv/41Sn2emQ0dycQW4uZXLejwKvg6EsvbdlVL+FYEct7A==";
      };
    };
  };
in
{
  "@volar/vue-language-server" = nodeEnv.buildNodePackage {
    name = "_at_volar_slash_vue-language-server";
    packageName = "@volar/vue-language-server";
    version = "1.6.5";
    src = fetchurl {
      url = "https://registry.npmjs.org/@volar/vue-language-server/-/vue-language-server-1.6.5.tgz";
      sha512 = "+/kSxBAkZbeVBePoG2qRSvCkVIslk1dNlU5wQHAcbzOFTi7pEQR6C+kjoj94I+vGXVGMFfDwOYm07sPsd8dM6w==";
    };
    dependencies = [
      sources."@babel/parser-7.22.14"
      sources."@emmetio/abbreviation-2.3.3"
      sources."@emmetio/css-abbreviation-2.1.8"
      sources."@emmetio/scanner-1.0.4"
      sources."@johnsoncodehk/pug-beautify-0.2.2"
      sources."@jridgewell/sourcemap-codec-1.4.15"
      sources."@volar-plugins/css-2.0.0"
      sources."@volar-plugins/emmet-2.0.0"
      sources."@volar-plugins/html-2.0.0"
      sources."@volar-plugins/json-2.0.0"
      sources."@volar-plugins/pug-2.0.0"
      sources."@volar-plugins/pug-beautify-2.0.0"
      sources."@volar-plugins/typescript-2.0.0"
      sources."@volar-plugins/typescript-twoslash-queries-2.0.0"
      sources."@volar/language-core-1.4.1"
      sources."@volar/language-server-1.4.1"
      sources."@volar/language-service-1.4.1"
      sources."@volar/source-map-1.4.1"
      sources."@volar/vue-language-core-1.6.5"
      sources."@volar/vue-language-service-1.6.5"
      (sources."@vscode/emmet-helper-2.9.2" // {
        dependencies = [
          sources."jsonc-parser-2.3.1"
          sources."vscode-uri-2.1.2"
        ];
      })
      sources."@vscode/l10n-0.0.11"
      sources."@vue/compiler-core-3.3.4"
      sources."@vue/compiler-dom-3.3.4"
      sources."@vue/compiler-sfc-3.3.4"
      sources."@vue/compiler-ssr-3.3.4"
      sources."@vue/reactivity-3.3.4"
      sources."@vue/reactivity-transform-3.3.4"
      sources."@vue/shared-3.3.4"
      sources."acorn-7.4.1"
      sources."balanced-match-1.0.2"
      sources."brace-expansion-2.0.1"
      sources."call-bind-1.0.2"
      sources."character-parser-2.2.0"
      sources."de-indent-1.0.2"
      sources."emmet-2.4.6"
      sources."estree-walker-2.0.2"
      sources."function-bind-1.1.1"
      sources."get-intrinsic-1.2.1"
      sources."has-1.0.3"
      sources."has-proto-1.0.1"
      sources."has-symbols-1.0.3"
      sources."has-tostringtag-1.0.0"
      sources."he-1.2.0"
      sources."is-expression-4.0.0"
      sources."is-regex-1.1.4"
      sources."jsonc-parser-3.2.0"
      sources."lru-cache-6.0.0"
      sources."magic-string-0.30.3"
      sources."minimatch-9.0.3"
      sources."muggle-string-0.2.2"
      sources."nanoid-3.3.6"
      sources."object-assign-4.1.1"
      sources."picocolors-1.0.0"
      sources."postcss-8.4.29"
      sources."pug-error-2.0.0"
      sources."pug-lexer-5.0.1"
      sources."pug-parser-6.0.0"
      sources."request-light-0.7.0"
      sources."semver-7.5.4"
      sources."source-map-js-1.0.2"
      sources."token-stream-1.0.0"
      sources."typesafe-path-0.2.2"
      sources."typescript-5.2.2"
      sources."typescript-auto-import-cache-0.2.1"
      (sources."vscode-css-languageservice-6.2.7" // {
        dependencies = [
          sources."@vscode/l10n-0.0.16"
        ];
      })
      (sources."vscode-html-languageservice-5.0.7" // {
        dependencies = [
          sources."@vscode/l10n-0.0.16"
        ];
      })
      (sources."vscode-json-languageservice-5.3.6" // {
        dependencies = [
          sources."@vscode/l10n-0.0.16"
        ];
      })
      sources."vscode-jsonrpc-8.1.0"
      sources."vscode-languageserver-8.1.0"
      sources."vscode-languageserver-protocol-3.17.3"
      sources."vscode-languageserver-textdocument-1.0.10"
      sources."vscode-languageserver-types-3.17.3"
      sources."vscode-nls-5.2.0"
      sources."vscode-uri-3.0.7"
      sources."vue-component-meta-1.6.5"
      sources."vue-component-type-helpers-1.6.5"
      sources."vue-template-compiler-2.7.14"
      sources."yallist-4.0.0"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      homepage = "https://github.com/vuejs/language-tools#readme";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  emmet-ls = nodeEnv.buildNodePackage {
    name = "emmet-ls";
    packageName = "emmet-ls";
    version = "0.4.2";
    src = fetchurl {
      url = "https://registry.npmjs.org/emmet-ls/-/emmet-ls-0.4.2.tgz";
      sha512 = "nwiUsbph9c4TkZjrKb7OqrUG6XQ3AxmmVn3IDR6FTx/xLLAegpmSxfOrvvPADbz9vOkSP6jjShpux1tNrqqIkQ==";
    };
    dependencies = [
      sources."@emmetio/abbreviation-2.3.3"
      sources."@emmetio/css-abbreviation-2.1.8"
      sources."@emmetio/scanner-1.0.4"
      sources."@types/node-17.0.45"
      sources."emmet-2.4.6"
      sources."typescript-4.9.5"
      sources."vscode-jsonrpc-6.0.0"
      sources."vscode-languageserver-7.0.0"
      sources."vscode-languageserver-protocol-3.16.0"
      sources."vscode-languageserver-textdocument-1.0.10"
      sources."vscode-languageserver-types-3.16.0"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "emmet support by LSP";
      homepage = "https://github.com/aca/emmet-ls#readme";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
  "@previewjs/cli" = nodeEnv.buildNodePackage {
    name = "_at_previewjs_slash_cli";
    packageName = "@previewjs/cli";
    version = "1.24.1";
    src = fetchurl {
      url = "https://registry.npmjs.org/@previewjs/cli/-/cli-1.24.1.tgz";
      sha512 = "LXAzwPrNaHmUbTmx0IbRcuOyZkpBV3PKN2UCfr+WUBbKypBypf7bxq5uDwKD6vwitxivRvRgclOK2oTxB9ulXA==";
    };
    buildInputs = globalBuildInputs;
    meta = {
      homepage = "https://previewjs.com";
      license = "https://previewjs.com/eula";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}
