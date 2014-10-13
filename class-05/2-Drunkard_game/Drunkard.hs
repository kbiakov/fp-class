{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Hearts | Diamonds | Clubs
            deriving (Show, Eq)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Show, Eq, Ord)

data Card = Card Value Suit
            deriving (Show, Eq)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2) = s1 == s2

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 _) `beats` (Card v2 _)
  | v1 > v2 = GT
  | v1 == v2 = EQ
  | otherwise = LT
  
{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round cards = game_round' (cards, [])
  where game_round' (((c1:cards1), (c2:cards2)), stack)
    | c1 `beats` c2 == GT = (cards1 ++ (c1:c2:stack), cards2)
    | c1 `beats` c2 == LT = (cards1, cards2 ++ (c1:c2:stack))
    | otherwise = game_round' ((cards1, cards2), c1:c2:stack)

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second

game :: ([Card], [Card]) -> (Winner, Int)
game cards = game' (cards, 0)
  where
    game' (([], _), count) = (Second, count)
    game' ((_, []), count) = (First, count)
    game' (cards, count) = game' (game_round cards, count + 1)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

cards1_from_pair1 = [
Card Three Clubs, Card Seven Clubs,
Card Four Spades, Joker ColorJoker,
Card Two Diamonds, Card Three Clubs,
Card King Clubs, Card Seven Hearts,
Card Queen Spades, Card Four Diamonds]

cards2_from_pair1 = [
Card King Spades, Card Seven Hearts,
Card Two Diamonds, Card Jack Clubs,
Card Six Hearts, Card Nine Clubs,
Card Ace Clubs, Card Seven Diamonds,
Card King Hearts, Card Eight Clubs]

cards1_from_pair2 = [
Card Three Clubs, Card Seven Clubs,
Card Four Spades, Joker ColorJoker,
Card Two Diamonds, Card Three Clubs,
Card King Clubs, Card Seven Hearts,
Card Queen Spades, Card Four Diamonds]

cards2_from_pair2 = [
Card King Spades, Card Seven Hearts,
Card Two Diamonds, Card Jack Clubs,
Card Six Hearts, Card Nine Clubs,
Card Ace Clubs, Card Seven Diamonds,
Card King Hearts, Card Eight Clubs]

cards1_from_pair3 = [
Card Three Clubs, Card Seven Clubs,
Card Four Spades, Joker ColorJoker,
Card Two Diamonds, Card Three Clubs,
Card King Clubs, Card Seven Hearts,
Card Queen Spades, Card Four Diamonds]

cards2_from_pair3 = [
Card King Spades, Card Seven Hearts,
Card Two Diamonds, Card Jack Clubs,
Card Six Hearts, Card Nine Clubs,
Card Ace Clubs, Card Seven Diamonds,
Card King Hearts, Card Eight Clubs]

cards1_from_pair4 = [
Card Three Clubs, Card Seven Clubs,
Card Four Spades, Joker ColorJoker,
Card Two Diamonds, Card Three Clubs,
Card King Clubs, Card Seven Hearts,
Card Queen Spades, Card Four Diamonds]

cards2_from_pair4 = [
Card King Spades, Card Seven Hearts,
Card Two Diamonds, Card Jack Clubs,
Card Six Hearts, Card Nine Clubs,
Card Ace Clubs, Card Seven Diamonds,
Card King Hearts, Card Eight Clubs]

cards1_from_pair5 = [
Card Queen Clubs, Card King Spades,
Card King Spades, Joker ColorJoker,
Card Nine Diamonds, Card King Hearts,
Card King Clubs, Joker GrayJoker,
Card Queen Spades, Card King Diamonds]

cards2_from_pair5 = [
Card Two Spades, Card Seven Hearts,
Card Two Diamonds, Card Four Clubs,
Card Six Hearts, Card Nine Clubs,
Card Nine Clubs, Card Five Diamonds,
Card Two Hearts, Card Five Clubs]

game_test1 = game (cards1_from_pair1, cards2_from_pair1)
game_test2 = game (cards1_from_pair2, cards2_from_pair2)
game_test3 = game (cards1_from_pair3, cards2_from_pair3)
game_test4 = game (cards1_from_pair4, cards2_from_pair4)
game_test5 = game (cards1_from_pair5, cards2_from_pair5)

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
