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
  TheSongStealer -> "唄を盗む者"
  TheEclipse -> "THE ECLIPSE"
  QuestionWhereAmINow -> "QUESTION: WHERE AM I NOW?"
  StarEcology -> "星の生態"
  Interception -> "迎撃"
  TheSavageMoonCat -> "THE SAVAGE MOON CAT"

type TextEvent = { text :: String, style :: String }

type WaitEvent = { time :: Number }

type ImageEvent = { src :: String, style :: String }

data NovelEvent
  = Text TextEvent
  | Wait WaitEvent
  | Image ImageEvent

type NovelContent = Array NovelEvent

te :: TextEvent
te = { text: "", style: "" }

we :: WaitEvent
we = { time: 2000.0 }

ie :: ImageEvent
ie = { src: "", style: "" }

excursionContent :: NovelContent
excursionContent =
  [ Image $ ie { src = "/assets/images/1.webp", style = "w-96" }
  , Wait we
  , Text $ te { text = "天の川の中をじっと凝視すると" }
  , Text $ te { text = "まっさかさまに輝く星々の航海が見えた" }
  , Text $ te { text = "むこうには　幸福な遊び場があるかしれないと思い" }
  , Text $ te { text = "私は星になって　ピュン！　と飛び出した" }
  ]

theSongStealerContent :: NovelContent
theSongStealerContent =
  [ Image $ ie { src = "/assets/images/2.webp" }
  , Wait we
  , Text $ te { text = "街角で　唄を見失っていると" }
  , Text $ te { text = "暗い小路に　薄荷水のとける音が光っていた" }
  , Text $ te { text = "自分は　このぼんやりとした白い音をひき抜いて" }
  , Text $ te { text = "二階の地平線から　口笛を吹いた" }
  ]

theEclipseContent :: NovelContent
theEclipseContent =
  [ Image $ ie { src = "/assets/images/3.webp" }
  , Wait we
  , Text $ te { text = "赤い月とナンセンスな太陽が　ドン！　と衝突した" }
  , Text $ te { text = "そのはずみに　月は捨てセリフを落とした" }
  , Text $ te { text = "捨てセリフは　ぬれたアスファルトの上に落ち" }
  , Text $ te { text = "地球が　ロマンチックの彼方へ　かち飛ばされた" }
  ]

questionWhereAmINowContent :: NovelContent
questionWhereAmINowContent =
  [ Image $ ie { src = "/assets/images/4.webp" }
  , Wait we
  , Text $ te { text = "A氏は夜更けのほうへ　流星をうたいながら歩いている" }
  , Text $ te { text = "月もプラタナスの鍵の方へ　ゆっくり動き始めた" }
  , Text $ te { text = "こうしてお月様は　第二の月の方へ自分を残してしまった" }
  ]

starEcologyContent :: NovelContent
starEcologyContent =
  [ Image $ ie { src = "/assets/images/5.webp" }
  , Wait we
  , Text $ te { text = "青い月の説によると" }
  , Text $ te { text = "星は理論において　いつも同じ位置に散乱するが" }
  , Text $ te { text = "千一時間に一度" }
  , Text $ te { text = "いい空気を吸おうと　パタパタと飛んで行って" }
  , Text $ te { text = "空を留守にしてしまうのだ" }
  ]

interceptionContent :: NovelContent
interceptionContent =
  [ Image $ ie { src = "/assets/images/6.webp" }
  , Wait we
  , Text $ te { text = "ある晩" }
  , Text $ te { text = "シグナルの街角から　唄を投げつけた" }
  , Text $ te { text = "唄は空から一メートル離れた所で　流星と衝突し" }
  , Text $ te { text = "三百六連発の花火が　街上で光った" }
  ]

theSavageMoonCatContent :: NovelContent
theSavageMoonCatContent =
  [ Image $ ie { src = "/assets/images/7.webp" }
  , Wait we
  , Text $ te { text = "登ってしまったお月様が　街路の中へ投げ込まれた" }
  , Text $ te { text = "その真ん中でまるい孔を開けると　黒猫の影が飛び出した" }
  , Text $ te { text = "そして　猫は暗い小路で" }
  , Text $ te { text = "鳥の腕を三つ取った" }
  ]

getContent :: NovelTitle -> NovelContent
getContent = case _ of
  Excursion -> excursionContent
  TheSongStealer -> theSongStealerContent
  TheEclipse -> theEclipseContent
  QuestionWhereAmINow -> questionWhereAmINowContent
  StarEcology -> starEcologyContent
  Interception -> interceptionContent
  TheSavageMoonCat -> theSavageMoonCatContent
