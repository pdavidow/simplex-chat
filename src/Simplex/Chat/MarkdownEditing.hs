{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}


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

    unchangers :: M.Map Int DiffUnchangedTextuallyStatus 
    unchangers = F.foldl' f mempty unchangedTextually
        where
        unchangedTextually :: Seq (Int, FormattedChar, FormattedChar) 
        unchangedTextually = g <$> S.zip leftWithoutDeletes rightWithoutInserts

        leftWithoutDeletes :: Seq (Int, FormattedChar) -- indexed in original left
        leftWithoutDeletes = S.filter (\(i, _) -> i `notElem` deleteIndicies) leftZ 
            where leftZ = S.zip (S.fromList [0 .. S.length left]) left

        rightWithoutInserts :: Seq (Int, FormattedChar) -- indexed in original right
        rightWithoutInserts = S.filter (\(i, _) -> i `notElem` insertIndicies) rightZ 
            where rightZ = S.zip (S.fromList [0 .. S.length right]) right

        f :: M.Map Int DiffUnchangedTextuallyStatus -> (Int, FormattedChar, FormattedChar) -> M.Map Int DiffUnchangedTextuallyStatus
        f acc (i, FormattedChar _ fL, FormattedChar _ fR) = M.insert i x acc
            where x = if fL == fR then Pristine else ChangedToFormat fR

        g :: ((Int, FormattedChar), (Int, FormattedChar)) -> (Int, FormattedChar, FormattedChar)
        g ((i,c), (_j,d)) = (i,c,d) -- i and _j should always be equal            

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
            D.EditInsert i m n -> S.take i' acc >< inserts >< S.drop i' acc 
         -- D.EditInsert i m n -> S.take i  acc >< inserts >< S.drop i  acc              
            -- if ok to have inserts before deletes, use i not i'
            -- Using i of course is faster, but i' approach perhaps can be optimised
              
                where 
                i' = slidePastDeleteBlock i

                slidePastDeleteBlock :: Int -> Int
                slidePastDeleteBlock x = case S.lookup x acc of
                    Nothing -> x
                    Just (DiffedChar _ diffStatus) -> 
                        if diffStatus == Deleted then slidePastDeleteBlock (x + 1) 
                        else x

                rightFormatChars = S.take (n - m + 1) $ S.drop m right
                inserts = fmap (`DiffedChar` Inserted) rightFormatChars
