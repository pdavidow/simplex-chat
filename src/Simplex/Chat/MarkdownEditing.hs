{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}


module Simplex.Chat.MarkdownEditing 
    ( DiffedChar(..)
    , DiffedPlainChar(..)
    , DiffStatus(..)
    , DiffPlainStatus(..)
    , DiffFormatStatus(..)
    , FormattedChar(..)
    , LeftSide(..)
    , RightSide(..)
    , findDiffs
    , findPlainDiffs
    )
    where


import qualified Data.Foldable as F
import           Data.Function ( (&) )
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Diff.Myers as D
import           Simplex.Chat.Markdown (Format)
import qualified Debug.Trace as DBG

data DiffStatus 
    = UnchangedChar DiffFormatStatus
    | Inserted 
    | Deleted 
    deriving (Show, Eq)


data DiffPlainStatus
    = UnchangedP
    | InsertedP
    | DeletedP
    deriving (Show, Eq)


data DiffFormatStatus
    = UnchangedFormat
    | ChangedToFormat (Maybe Format)
    deriving (Show, Eq)


data DiffedChar = DiffedChar FormattedChar DiffStatus
    deriving (Show, Eq)


data DiffedPlainChar = DiffedPlainChar Char DiffPlainStatus
    deriving (Show, Eq)


data FormattedChar = FormattedChar 
    { char :: Char
    , format :: Maybe Format
    }
    deriving (Show, Eq)


newtype LeftSide  a = LeftSide  a deriving (Show, Eq)
newtype RightSide a = RightSide a deriving (Show, Eq)


newtype DeleteIndicies = DeleteIndicies (Seq Int) deriving (Show, Eq)
newtype InsertIndicies = InsertIndicies (Seq Int) deriving (Show, Eq)


findPlainDiffs :: LeftSide T.Text -> RightSide T.Text -> Seq DiffedPlainChar
findPlainDiffs (LeftSide left) (RightSide right) = toPlain <$> diffs
    where
    diffs = findDiffs (LeftSide $ toFormatted left) (RightSide $ toFormatted right)

    toPlain :: DiffedChar -> DiffedPlainChar
    toPlain (DiffedChar (FormattedChar c _) diffStatus) = DiffedPlainChar c diffStatusPlain
        where 
        diffStatusPlain = case diffStatus of
            UnchangedChar _ -> UnchangedP
            Inserted -> InsertedP
            Deleted -> DeletedP

    toFormatted :: T.Text -> Seq FormattedChar
    toFormatted = fmap (`FormattedChar` Nothing) . S.fromList . T.unpack             


findDiffs :: LeftSide (Seq FormattedChar) -> RightSide (Seq FormattedChar) -> Seq DiffedChar
findDiffs (LeftSide left) (RightSide right) = addInserts markDeletesAndUnchangedChars
    where
    edits = D.diffTexts (toText left) (toText right)  
    
    toText :: Seq FormattedChar -> T.Text
    toText = T.pack . F.toList . fmap char 

    -- unchangedChars :: M.Map Int DiffFormatStatus 
    -- unchangedChars = F.foldl' f mempty unchangedCharPairs
    --     where
    --     unchangedCharPairs :: Seq (Int, FormattedChar, FormattedChar) 
    --     unchangedCharPairs = g <$> S.zip leftWithoutDeletes rightWithoutInserts

    --     leftWithoutDeletes :: Seq (Int, FormattedChar) -- indexed in original left
    --     leftWithoutDeletes = S.filter (\(i, _) -> i `notElem` deleteIndicies) leftZ 
    --         where leftZ = S.zip (S.fromList [0 .. S.length left]) left

    --     rightWithoutInserts :: Seq (Int, FormattedChar) -- indexed in original right
    --     rightWithoutInserts = S.filter (\(i, _) -> i `notElem` insertIndicies) rightZ 
    --         where rightZ = S.zip (S.fromList [0 .. S.length right]) right

    --     f :: M.Map Int DiffFormatStatus -> (Int, FormattedChar, FormattedChar) -> M.Map Int DiffFormatStatus
    --     f acc (i, FormattedChar _ fL, FormattedChar _ fR) = M.insert i x acc
    --         where x = if fL == fR then UnchangedFormat else ChangedToFormat fR

    --     g :: ((Int, FormattedChar), (Int, FormattedChar)) -> (Int, FormattedChar, FormattedChar)
    --     g ((i,c), (_j,d)) = (i,c,d) -- i and _j should always be equal            

    -- markDeletesAndUnchangedChars :: Seq DiffedChar
    -- markDeletesAndUnchangedChars = S.mapWithIndex f left
    --     where
    --     f :: Int -> FormattedChar -> DiffedChar
    --     f i x = DiffedChar x $
    --         if i `elem` deleteIndicies then Deleted 
    --         else UnchangedChar $ unchangedChars M.! i -- should never error             

    markDeletesAndUnchangedChars :: Seq DiffedChar
    markDeletesAndUnchangedChars = S.mapWithIndex f left
        where
        f :: Int -> FormattedChar -> DiffedChar
        f i x = DBG.trace ("f i x: "<> show (i, x)) $ DiffedChar x $
            if i `elem` deleteIndicies then Deleted     
            else UnchangedChar $ status i

        indices :: (DeleteIndicies, InsertIndicies)
        indices = DBG.trace ("indices: " <> show (F.foldl' g (DeleteIndicies S.empty, InsertIndicies S.empty) edits)) $ F.foldl' g (DeleteIndicies S.empty, InsertIndicies S.empty) edits
            where
            g :: (DeleteIndicies, InsertIndicies) -> D.Edit -> (DeleteIndicies, InsertIndicies)
            g (x@(DeleteIndicies ds), y@(InsertIndicies is)) e = case e of
                D.EditDelete   m n -> (x', y)  where x' = DeleteIndicies $ ds >< S.fromList [m .. n]  
                D.EditInsert _ m n -> (x , y') where y' = InsertIndicies $ is >< S.fromList [m .. n] 

        (DeleteIndicies deleteIndicies, InsertIndicies insertIndicies) = indices 

        status :: Int -> DiffFormatStatus
        status i = DBG.trace ("status i: "<> show i) $ if fL == fR then UnchangedFormat else ChangedToFormat fR
            where
            (FormattedChar _ fL) = leftWithoutDeletes  M.! i
            (FormattedChar _ fR) = rightWithoutInserts M.! i        

        leftWithoutDeletes :: M.Map Int FormattedChar
        leftWithoutDeletes = DBG.trace ("leftWithoutDeletes: "<> show (
            left
            & S.zip (S.fromList [0 .. (S.length left - 1)])
            & S.filter (\(i, _) -> i `notElem` deleteIndicies)
            & F.toList
            & M.fromAscList            
            )) $ 
            left
            & S.zip (S.fromList [0 .. (S.length left - 1)])
            & S.filter (\(i, _) -> i `notElem` deleteIndicies)
            & F.toList
            & M.fromAscList

        rightWithoutInserts :: M.Map Int FormattedChar
        rightWithoutInserts = DBG.trace ("rightWithoutInserts: "<> show (
            right
            & S.zip (S.fromList [0 .. S.length right])
            & S.filter (\(i, _) -> i `notElem` insertIndicies)
            & F.toList
            & M.fromAscList  
            )) $             
            right
            & S.zip (S.fromList [0 .. S.length right])
            & S.filter (\(i, _) -> i `notElem` insertIndicies)
            & F.toList
            & M.fromAscList            

    addInserts :: Seq DiffedChar -> Seq DiffedChar
    addInserts base = F.foldr f base edits -- start from end and work backwards, hence foldr
        where
        f :: D.Edit -> Seq DiffedChar -> Seq DiffedChar
        f e acc = case e of
            D.EditDelete _ _ -> acc
            D.EditInsert i m n -> S.take i' acc >< inserts >< S.drop i' acc 
         -- D.EditInsert i m n -> S.take i  acc >< inserts >< S.drop i  acc              
            -- if ok to have inserts before deletes, use i not i'
            -- Using i of course is faster, but perhaps i' approach can be optimised
              
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