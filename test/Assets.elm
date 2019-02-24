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
        |> Decode.andMap
            (Decode.field "icons"
                (Decode.succeed Icons
                    |> Decode.andMap (Decode.field "arrowUpSvg" Decode.string)
                    |> Decode.andMap (Decode.field "arrowDownSvg" Decode.string)
                )
            )
        |> Decode.andMap
            (Decode.field "fonts"
                (Decode.succeed Fonts
                    |> Decode.andMap (Decode.field "gothamOtf" Decode.string)
                )
            )
