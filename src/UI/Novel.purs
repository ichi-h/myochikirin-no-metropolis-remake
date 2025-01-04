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
