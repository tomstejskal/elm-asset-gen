# elm-asset-gen

Generate assets module for your Elm application from content of a directory.

## Usage

Change to a directory where you want to create the assets module (i.e. src directory of your Elm project).
Then call the application with a path to the assets directory as its argument.

```
cd src
elm-asset-gen ../assets
```

This will create two files in the `src` directory:

* `Assets.js`
* `Assets.elm`

## Assets.js

The javascript module will contain imports and export of all the assets.

If the structure of the assets directory is like this:
```
.
├── fonts
│   └── Gotham.otf
└── icons
    ├── arrow-down.svg
    └── arrow-up.svg
```

the `Assets.js` module will contain:

```js
import arrowUpSvg from "../assets/icons/arrow-up.svg";
import arrowDownSvg from "../assets/icons/arrow-down.svg";
import gothamOtf from "../assets/fonts/Gotham.otf";

export default {
  icons: {
    arrowUpSvg: arrowUpSvg,
    arrowDownSvg: arrowDownSvg
  },
  fonts: {
    gothamOtf: gothamOtf
  }
};
```

## Assets.elm

The Elm module contains a type alias for the assets record and a JSON decoder for this type:

```elm
module Assets exposing (Assets, decoder)

import Json.Decode as Decode
import Json.Decode.Extra as Decode


type alias Icons =
    { arrowUpSvg : String
    , arrowDownSvg : String
    }


type alias Fonts =
    { gothamOtf : String
    }


type alias Assets =
    { icons : Icons
    , fonts : Fonts
    }


decoder : Decode.Decoder Assets
decoder =
    Decode.succeed Assets
        |> Decode.andMap (Decode.field "icons" (Decode.succeed Icons
            |> Decode.andMap (Decode.field "arrowUpSvg" Decode.string)
            |> Decode.andMap (Decode.field "arrowDownSvg" Decode.string)
        ))
        |> Decode.andMap (Decode.field "fonts" (Decode.succeed Fonts
            |> Decode.andMap (Decode.field "gothamOtf" Decode.string)
        ))
```
