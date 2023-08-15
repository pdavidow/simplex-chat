{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}


module Simplex.Chat.MarkdownEditing 
    ( DiffedChar(..)
    , DiffStatus(..)
    , DiffUnchangedTextuallyStatus(..)
    , FormattedChar(..)
    , LeftSide(..)
    , RightSide(..)
    , findDiffs
    )
    where


import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Sequence ( Seq(..), (><) )
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Diff.Myers as D
import           Simplex.Chat.Markdown ( FormattedText(..), Format )


data DiffStatus 
    = UnchangedTextually DiffUnchangedTextuallyStatus
    | Inserted 
    | Deleted 
    deriving (Show, Eq)


data DiffUnchangedTextuallyStatus
    = Pristine
    | ChangedToFormat (Maybe Format)
    deriving (Show, Eq)


data DiffedChar = DiffedChar FormattedChar DiffStatus
    deriving (Show, Eq)


data FormattedChar = FormattedChar 
    { char :: Char
    , format :: Maybe Format
    }
    deriving (Show, Eq)


newtype LeftSide  = LeftSide  (Seq FormattedChar) deriving (Show, Eq)
newtype RightSide = RightSide (Seq FormattedChar) deriving (Show, Eq)


newtype DeleteIndicies = DeleteIndicies (Seq Int) deriving (Show, Eq)
newtype InsertIndicies = InsertIndicies (Seq Int) deriving (Show, Eq)


toFormattedChars :: [FormattedText] -> [FormattedChar]
toFormattedChars = concatMap toChars
    where toChars (FormattedText f t) = map (`FormattedChar` f) $ T.unpack t


toText :: Seq FormattedChar -> T.Text
toText = T.pack . F.toList . fmap char 


indicesFromEdits :: Seq D.Edit -> (DeleteIndicies, InsertIndicies)
indicesFromEdits = F.foldl' f (DeleteIndicies S.empty, InsertIndicies S.empty) 
    where
    f :: (DeleteIndicies, InsertIndicies) -> D.Edit -> (DeleteIndicies, InsertIndicies)
    f (x@(DeleteIndicies ds), y@(InsertIndicies is)) e = case e of
        D.EditDelete   m n -> (x', y)  where x' = DeleteIndicies $ ds >< S.fromList [m .. n]  
        D.EditInsert _ m n -> (x , y') where y' = InsertIndicies $ is >< S.fromList [m .. n] 


findDiffs :: LeftSide -> RightSide -> Seq DiffedChar
findDiffs (LeftSide left) (RightSide right) = addInserts markDeletesAndUnchangedTextually
    where
    edits = D.diffTexts (toText left) (toText right)  
    (DeleteIndicies deleteIndicies, InsertIndicies insertIndicies) = indicesFromEdits edits
        
    unchangedTextually :: Seq (Int, FormattedChar, FormattedChar) -- indexed in original
    unchangedTextually = f <$> S.zip leftWithoutDeletes rightWithoutInserts
        where
        leftWithoutDeletes :: Seq (Int, FormattedChar) -- indexed in original
        leftWithoutDeletes = S.filter (\(i, _) -> i `notElem` deleteIndicies) leftZ 
            where leftZ = S.zip (S.fromList [0 .. S.length left]) left

        rightWithoutInserts :: Seq (Int, FormattedChar) -- indexed in original
        rightWithoutInserts = S.filter (\(i, _) -> i `notElem` insertIndicies) rightZ 
            where rightZ = S.zip (S.fromList [0 .. S.length right]) right

        f :: ((Int, FormattedChar), (Int, FormattedChar)) -> (Int, FormattedChar, FormattedChar)
        f ((i,c), (j,d)) = (i,c,d) -- i and j should always be equal

    unchangers :: M.Map Int DiffUnchangedTextuallyStatus
    unchangers = F.foldl' f M.empty unchangedTextually
        where
        f :: M.Map Int DiffUnchangedTextuallyStatus -> (Int, FormattedChar, FormattedChar) -> M.Map Int DiffUnchangedTextuallyStatus
        f acc (i, FormattedChar _ fL, FormattedChar _ fR) = M.insert i x acc
            where x = if fL == fR then Pristine else ChangedToFormat fR

    markDeletesAndUnchangedTextually :: Seq DiffedChar
    markDeletesAndUnchangedTextually = S.mapWithIndex f left
        where
            f :: Int -> FormattedChar -> DiffedChar
            f i x = DiffedChar x $
                if i `elem` deleteIndicies then Deleted 
                else UnchangedTextually $ unchangers M.! i -- should never error

    addInserts :: Seq DiffedChar -> Seq DiffedChar
    addInserts base = F.foldr f base edits -- start from end and work backwards, hence foldr
        where
        f :: D.Edit -> Seq DiffedChar -> Seq DiffedChar
        f e acc = case e of
            D.EditDelete _ _ -> acc
            D.EditInsert i m n -> S.take i acc >< inserts >< S.drop i acc
                where 
                rightFormatChars = S.take (n - m + 1) $ S.drop m right
                inserts = fmap (`DiffedChar` Inserted) rightFormatChars
