module UI.Novel where

import Prelude

data NovelTitle
  = Excursion
  | TheSongStealer
  | TheEclipse
  | QuestionWhereAmINow
  | StarEcology
  | Interception
  | TheSavageMoonCat

derive instance eqNovelTitle :: Eq NovelTitle

toString :: NovelTitle -> String
toString = case _ of
  Excursion -> "遠足"
  TheSongStealer -> "唄を盗む人"
  TheEclipse -> "THE ECLIPSE"
  QuestionWhereAmINow -> "QUESTION: WHERE AM I NOW?"
  StarEcology -> "星の生態"
  Interception -> "迎撃"
  TheSavageMoonCat -> "THE SAVAGE MOON CAT"

type MessageEvent = { text :: String, style :: String, waitClick :: Boolean, clear :: Boolean }

type WaitEvent = { time :: Number }

type ImageEvent = { src :: String, style :: String }

type TextEvent = { text :: String, style :: String }

type BaseLayerEvent = { style :: String }

type MessageBoxEvent = { style :: String }

data NovelEvent
  = Message MessageEvent
  | Wait WaitEvent
  | Image ImageEvent
  | Text TextEvent
  | BaseLayer BaseLayerEvent
  | MessageBox MessageBoxEvent

me :: MessageEvent
me = { text: "", style: "leading-normal", waitClick: true, clear: false }

we :: WaitEvent
we = { time: 1000.0 }

ie :: ImageEvent
ie = { src: "", style: "max-lg:w-full max-2xl:w-160 w-224" }

te :: TextEvent
te = { text: "", style: "font-pigmo text-secondary max-md:text-3xl max-2xl:text-4xl text-6xl" }

ble :: BaseLayerEvent
ble = { style: "w-full h-full flex flex-col justify-center items-center max-sm:gap-6 gap-8 max-sm:mx-4 max-lg:mx-16" }

mbe :: MessageBoxEvent
mbe = { style: "text-secondary w-full max-xs:text-base max-sm:text-lg max-2xl:text-2xl text-4xl max-md:text-left text-center break-all " }

excursionContent :: Array NovelEvent
excursionContent =
  [ BaseLayer ble
  , MessageBox $ mbe { style = mbe.style <> "max-2xl:h-36 h-56" }
  , Image $ ie { src = "/assets/images/1.webp" }
  , Text $ te { text = toString Excursion }
  , Wait we
  , Message $ me { text = "天の川の中をじっと凝視すると" }
  , Message $ me { text = "まっさかさまに輝く星々の航海が見えた" }
  , Message $ me { text = "むこうには　幸福な遊び場があるかしれないと思い" }
  , Message $ me { text = "私は星になって　ピュン！　と飛び出した" }
  ]

theSongStealerContent :: Array NovelEvent
theSongStealerContent =
  [ BaseLayer ble
  , MessageBox $ mbe { style = mbe.style <> "max-xs:h-36 max-sm:h-40 max-2xl:h-36 h-56" }
  , Image $ ie { src = "/assets/images/2.webp" }
  , Text $ te { text = toString TheSongStealer }
  , Wait we
  , Message $ me { text = "街角で　唄を見失っていると" }
  , Message $ me { text = "暗い小路に　薄荷水のとける音が光っていた" }
  , Message $ me { text = "自分は　このぼんやりとした白い音をひき抜いて" }
  , Message $ me { text = "二階の地平線から　口笛を吹いた" }
  ]

theEclipseContent :: Array NovelEvent
theEclipseContent =
  [ BaseLayer ble
  , MessageBox $ mbe { style = mbe.style <> "max-xs:h-48 max-sm:h-44 max-2xl:h-36 h-56" }
  , Image $ ie { src = "/assets/images/3.webp" }
  , Text $ te { text = toString TheEclipse }
  , Wait we
  , Message $ me { text = "赤い月とナンセンスな太陽が　ドン！　と衝突した" }
  , Message $ me { text = "そのはずみに　月は捨てセリフを落とした" }
  , Message $ me { text = "捨てセリフは　ぬれたアスファルトの上に落ち" }
  , Message $ me { text = "地球が　ロマンチックの彼方へ　かち飛ばされた" }
  ]

questionWhereAmINowContent :: Array NovelEvent
questionWhereAmINowContent =
  [ BaseLayer ble
  , MessageBox $ mbe { style = mbe.style <> "max-xs:h-36 max-sm:h-40 max-2xl:h-28 h-40" }
  , Image $ ie { src = "/assets/images/4.webp" }
  , Text $ te { text = toString QuestionWhereAmINow }
  , Wait we
  , Message $ me { text = "A氏は夜更けのほうへ　流星をうたいながら歩いている" }
  , Message $ me { text = "月もプラタナスの鍵の方へ　ゆっくり動き始めた" }
  , Message $ me { text = "こうしてお月様は　第二の月の方へ自分を残してしまった" }
  ]

starEcologyContent :: Array NovelEvent
starEcologyContent =
  [ BaseLayer ble
  , MessageBox $ mbe { style = mbe.style <> "max-xs:h-40 max-sm:h-44 max-2xl:h-44 h-64" }
  , Image $ ie { src = "/assets/images/5.webp" }
  , Text $ te { text = toString StarEcology }
  , Wait we
  , Message $ me { text = "青い月の説によると" }
  , Message $ me { text = "星は理論において　いつも同じ位置に散乱するが" }
  , Message $ me { text = "千時間に一度" }
  , Message $ me { text = "いい空気を吸おうと　パタパタと飛んで行って" }
  , Message $ me { text = "空を留守にしてしまうのだ" }
  ]

interceptionContent :: Array NovelEvent
interceptionContent =
  [ BaseLayer ble
  , MessageBox $ mbe { style = mbe.style <> "max-xs:h-28 max-sm:h-32 max-2xl:h-36 h-56" }
  , Image $ ie { src = "/assets/images/6.webp" }
  , Text $ te { text = toString Interception }
  , Wait we
  , Message $ me { text = "ある晩" }
  , Message $ me { text = "シグナルの街角から　唄を投げつけた" }
  , Message $ me { text = "唄は空から一メートル離れた所で　流星と衝突し" }
  , Message $ me { text = "三百六連発の花火が　街上で光った" }
  ]

theSavageMoonCatContent :: Array NovelEvent
theSavageMoonCatContent =
  [ BaseLayer ble
  , MessageBox $ mbe { style = mbe.style <> "max-xs:h-36 max-sm:h-40 max-2xl:h-36 h-56" }
  , Image $ ie { src = "/assets/images/7.webp" }
  , Text $ te { text = toString TheSavageMoonCat }
  , Wait we
  , Message $ me { text = "登ってしまったお月様が　街路の中へ投げ込まれた" }
  , Message $ me { text = "その真ん中でまるい孔を開けると　黒猫の影が飛び出した" }
  , Message $ me { text = "そして　猫は暗い小路で" }
  , Message $ me { text = "鳥の腕を三つ取った" }
  ]

getContent :: NovelTitle -> Array NovelEvent
getContent = case _ of
  Excursion -> excursionContent
  TheSongStealer -> theSongStealerContent
  TheEclipse -> theEclipseContent
  QuestionWhereAmINow -> questionWhereAmINowContent
  StarEcology -> starEcologyContent
  Interception -> interceptionContent
  TheSavageMoonCat -> theSavageMoonCatContent
