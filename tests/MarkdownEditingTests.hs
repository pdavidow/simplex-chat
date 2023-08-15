{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownEditingTests where

import qualified Data.Sequence as S
import Simplex.Chat.Markdown
    ( colored, Format(Secret, Italic, Bold) )
import Simplex.Chat.MarkdownEditing
    ( FormattedChar(FormattedChar),
      DiffedChar(DiffedChar),
      DiffStatus(UnchangedTextually, Inserted, Deleted),
      findDiffs,
      DiffUnchangedTextuallyStatus(ChangedToFormat, Pristine),
      LeftSide(..),
      RightSide(..) )
import System.Console.ANSI.Types
import Test.Hspec


markdownEditingTests :: Spec
markdownEditingTests = do
  formattedEditedTextTests


formattedEditedTextTests :: Spec
formattedEditedTextTests = describe "show edits" do
  it "no change" do
    findDiffs 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing                                          
          ])   

        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing                                                            
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine                                                              
        ]


  it "add 1 char to empty" do
    findDiffs 
        (LeftSide $ S.fromList
          [                                          
          ])   

        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing                                                            
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) Inserted                                                                 
        ]
     

  it "del the one and only" do
    findDiffs 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing                                            
          ])   

        (RightSide $ S.fromList
          [                                                             
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) Deleted                                                                 
        ]


  it "one character change" do
    findDiffs 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   

        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                                              
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'r' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted      
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedTextually Pristine                                                        
        ]


  it "more1" do
    findDiffs 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   

        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing            
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' Nothing                                                    
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'r' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted      
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'l' Nothing) $ UnchangedTextually Pristine 
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedTextually Pristine                                       
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' Nothing) Inserted                   
        ]


  it "more2" do
    findDiffs 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   

        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing  
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' Nothing             
          , FormattedChar 'o' Nothing                                         
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' Nothing) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'r' Nothing) Deleted
        , DiffedChar (FormattedChar 'l' Nothing) Deleted  
        , DiffedChar (FormattedChar 'l' Nothing) Deleted          
        , DiffedChar (FormattedChar 'e' Nothing) Inserted
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' Nothing) Inserted
        , DiffedChar (FormattedChar 'o' Nothing) $ UnchangedTextually Pristine                      
        ]


  it "more3" do
    findDiffs 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' (Just Bold)    
          , FormattedChar 'H' (Just Bold)          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' (Just Secret)           
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' (Just $ colored Green)                                       
          ])   

        (RightSide $ S.fromList
          [ FormattedChar 'H' (Just Italic)   
          , FormattedChar 'H' (Just Bold)          
          , FormattedChar 'e' (Just $ colored Cyan)    
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing                 
          , FormattedChar 'z' (Just Secret)                  
          , FormattedChar 'o' (Just $ colored Blue)                                         
          ])  

      `shouldBe` S.fromList
        [ DiffedChar (FormattedChar 'H' (Just Bold)) $ UnchangedTextually (ChangedToFormat (Just Italic)) 
        , DiffedChar (FormattedChar 'H' (Just Bold)) $ UnchangedTextually Pristine
        , DiffedChar (FormattedChar 'r' Nothing) Deleted
        , DiffedChar (FormattedChar 'l' (Just Secret)) Deleted  
        , DiffedChar (FormattedChar 'l' Nothing) Deleted         
        , DiffedChar (FormattedChar 'e' (Just $ colored Cyan)) Inserted
        , DiffedChar (FormattedChar 'x' Nothing) Inserted
        , DiffedChar (FormattedChar 'y' Nothing) Inserted
        , DiffedChar (FormattedChar 'z' (Just Secret)) Inserted 
        , DiffedChar (FormattedChar 'o' (Just $ colored Green)) $ UnchangedTextually (ChangedToFormat (Just $ colored Blue))                                  
        ]


  it "more4" do
    findDiffs 
        (LeftSide $ S.fromList
          [ FormattedChar 'H' Nothing          
          , FormattedChar 'r' Nothing                 
          , FormattedChar 'l' Nothing      
          , FormattedChar '~' Nothing  
          , FormattedChar '!' Nothing  
          , FormattedChar '@' Nothing                                     
          , FormattedChar 'l' Nothing                  
          , FormattedChar 'o' Nothing                                    
          ])   

        (RightSide $ S.fromList
          [ FormattedChar 'H' Nothing           
          , FormattedChar 'e' Nothing 
          , FormattedChar 'r' Nothing             
          , FormattedChar 'x' Nothing                 
          , FormattedChar 'y' Nothing     
          , FormattedChar '!' Nothing  
          , FormattedChar '@' Nothing                       
          , FormattedChar 'z' Nothing             
          , FormattedChar 'o' Nothing      
          , FormattedChar '1' Nothing 
          , FormattedChar '2' Nothing                                                        
          ])  

      `shouldBe` S.fromList
          [ DiffedChar (FormattedChar 'H' Nothing) (UnchangedTextually Pristine)
          , DiffedChar (FormattedChar 'e' Nothing) Inserted
          , DiffedChar (FormattedChar 'r' Nothing) (UnchangedTextually Pristine)
          , DiffedChar (FormattedChar 'l' Nothing) Deleted
          , DiffedChar (FormattedChar '~' Nothing) Deleted
          , DiffedChar (FormattedChar 'x' Nothing) Inserted
          , DiffedChar (FormattedChar 'y' Nothing) Inserted
          , DiffedChar (FormattedChar '!' Nothing) (UnchangedTextually Pristine)
          , DiffedChar (FormattedChar '@' Nothing) (UnchangedTextually Pristine)
          , DiffedChar (FormattedChar 'l' Nothing) Deleted
          , DiffedChar (FormattedChar 'z' Nothing) Inserted
          , DiffedChar (FormattedChar 'o' Nothing) (UnchangedTextually Pristine)
          , DiffedChar (FormattedChar '1' Nothing) Inserted
          , DiffedChar (FormattedChar '2' Nothing) Inserted
          ]
