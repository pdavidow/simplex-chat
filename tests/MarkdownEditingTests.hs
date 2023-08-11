{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownEditingTests where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Simplex.Chat.Markdown
import Simplex.Chat.MarkdownEditing
import System.Console.ANSI.Types
import Test.Hspec

markdownEditingTests :: Spec
markdownEditingTests = do
  formattedEditedTextTests


plainText :: Text -> FormattedText
plainText = FormattedText Nothing

redText :: Text -> FormattedText
redText = FormattedText $ Just $ colored Red

plainEdited :: Text -> Bool -> EditedText
plainEdited t added = EditedText Nothing t (Just added)

redEdited :: Text -> Bool -> EditedText
redEdited t added = EditedText (Just $ colored Red) t (Just added)

formattedEditedTextTests :: Spec
formattedEditedTextTests = describe "show edits using Myers Diff algorithm" do
  it "no change" do
    formattedEditedText [plainText "H"] [plainText "H"]
      `shouldBe` 
        [ EditedChar Nothing 'H' Nothing              
        ] 

  it "add 1 char to empty" do
    formattedEditedText [plainText ""] [plainText "H"]
      `shouldBe` 
        [ EditedChar Nothing 'H' $ Just Add              
        ] 

  it "del the one and only" do
    formattedEditedText [plainText "H"] [plainText ""]
      `shouldBe` 
        [ EditedChar Nothing 'H' $ Just Delete              
        ] 

  it "one character change" do
    formattedEditedText [plainText "Hrllo"] [plainText "Hello"]
      `shouldBe` 
        [ EditedChar Nothing 'H' Nothing
        , EditedChar Nothing 'r' $ Just Delete
        , EditedChar Nothing 'e' $ Just Add
        , EditedChar Nothing 'l' Nothing
        , EditedChar Nothing 'l' Nothing
        , EditedChar Nothing 'o' Nothing                
        ]
        -- [ EditedChar Nothing 'H' Nothing
        -- , EditedChar Nothing 'e' $ Just Substitute
        -- , EditedChar Nothing 'l' Nothing
        -- , EditedChar Nothing 'l' Nothing
        -- , EditedChar Nothing 'o' Nothing                
        -- ]        

  it "more1" do
    formattedEditedText [plainText "Hrllo"] [plainText "Helloxyz"]
      `shouldBe` 
        [ EditedChar Nothing 'H' Nothing
        , EditedChar Nothing 'r' $ Just Delete
        , EditedChar Nothing 'e' $ Just Add
        , EditedChar Nothing 'l' Nothing
        , EditedChar Nothing 'l' Nothing
        , EditedChar Nothing 'o' Nothing       
        , EditedChar Nothing 'x' $ Just Add
        , EditedChar Nothing 'y' $ Just Add
        , EditedChar Nothing 'z' $ Just Add                                 
        ]        

  it "more2" do
    formattedEditedText [plainText "Hrllo"] [plainText "Hexyzo"]
      `shouldBe` 
        [ EditedChar Nothing 'H' Nothing
        , EditedChar Nothing 'e' $ Just Add  
        , EditedChar Nothing 'x' $ Just Add
        , EditedChar Nothing 'y' $ Just Add
        , EditedChar Nothing 'z' $ Just Add     
        , EditedChar Nothing 'r' $ Just Delete
        , EditedChar Nothing 'l' $ Just Delete
        , EditedChar Nothing 'l' $ Just Delete        
        , EditedChar Nothing 'o' Nothing                                      
        ] 

  it "more3" do
    diff 
        [ EditedChar (Just Bold)              'H' Nothing
        , EditedChar Nothing                  'r' Nothing
        , EditedChar (Just Secret)            'l' Nothing
        , EditedChar Nothing                  'l' Nothing
        , EditedChar (Just $ colored Green)   'o' Nothing                                       
        ]   
        [ EditedChar (Just Italic)            'H' Nothing
        , EditedChar (Just $ colored Cyan)    'e' Nothing
        , EditedChar Nothing                  'x' Nothing
        , EditedChar Nothing                  'y' Nothing
        , EditedChar (Just Secret)            'z' Nothing        
        , EditedChar (Just $ colored Blue)    'o' Nothing                                       
        ]    

      `shouldBe` 
        [ EditedChar (Just Italic)            'H' Nothing
        , EditedChar Nothing                  'e' $ Just Add  
        , EditedChar Nothing                  'x' $ Just Add
        , EditedChar Nothing                  'y' $ Just Add
        , EditedChar (Just Secret)            'z' $ Just Add     
        , EditedChar Nothing                  'r' $ Just Delete
        , EditedChar (Just Secret)            'l' $ Just Delete
        , EditedChar Nothing                  'l' $ Just Delete        
        , EditedChar (Just $ colored Blue)    'o' Nothing                                      
        ] 