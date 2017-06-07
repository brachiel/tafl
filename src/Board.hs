{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Board where

import Control.Monad (liftM)

-- |A square is maybe a piece (or nothing i.e. empty)
type Square p = Maybe p

-- |A square list is a list of squares
type SquareList p = [Square p]

-- |A list board is a simple (and maybe inefficient) way to store a 2 dimensional board
-- It is a list of lists of squares, i.e. [[Maybe Piece]]
data ListBoard p = ListBoard [SquareList p]

class Piece p where
    readPiece :: Char -> Maybe p
    showPiece :: p -> Char


class Piece p => Board b p | b -> p where
    readBoard :: String -> Maybe b

    showBoard :: b -> String

-- Chess

initialChessBoard = unlines $ [ "rnbqkbnr"
                              , "pppppppp"
                              , "........"
                              , "........"
                              , "........"
                              , "........"
                              , "PPPPPPPP"
                              , "RNBQKBNR"
                              ]

data ChessType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq, Ord)
data ChessColor = White | Black deriving (Show, Eq, Ord)
data ChessPiece = Piece ChessColor ChessType deriving (Show, Eq, Ord)

instance Piece ChessPiece where
    readPiece 'P' = Just $ Piece White Pawn
    readPiece 'N' = Just $ Piece White Knight
    readPiece 'B' = Just $ Piece White Bishop
    readPiece 'R' = Just $ Piece White Rook
    readPiece 'Q' = Just $ Piece White Queen
    readPiece 'K' = Just $ Piece White King
    readPiece 'p' = Just $ Piece Black Pawn
    readPiece 'n' = Just $ Piece Black Knight
    readPiece 'b' = Just $ Piece Black Bishop
    readPiece 'r' = Just $ Piece Black Rook
    readPiece 'q' = Just $ Piece Black Queen
    readPiece 'k' = Just $ Piece Black King
    readPiece _ = Nothing

    showPiece (Piece White Pawn  ) = 'P'
    showPiece (Piece White Knight) = 'N'
    showPiece (Piece White Bishop) = 'B'
    showPiece (Piece White Rook  ) = 'R'
    showPiece (Piece White Queen ) = 'Q'
    showPiece (Piece White King  ) = 'K'
    showPiece (Piece Black Pawn  ) = 'p'
    showPiece (Piece Black Knight) = 'n'
    showPiece (Piece Black Bishop) = 'b'
    showPiece (Piece Black Rook  ) = 'r'
    showPiece (Piece Black Queen ) = 'q'
    showPiece (Piece Black King  ) = 'k'

instance Piece p => Board (ListBoard p) p where
    readBoard = fmap ListBoard . (mapM readLine) . lines
                where readLine :: Piece p => [Char] -> Maybe (SquareList p)
                      readLine = mapM readSquare
                      readSquare :: Piece p => Char -> Maybe (Square p)
                      readSquare '.' = Just Nothing
                      readSquare a = case (readPiece a) of
                                        Just b    -> Just (Just b)
                                        otherwise -> Nothing

    showBoard (ListBoard bs) = unlines . map showLine $ bs
                where showLine :: Piece p => SquareList p -> [Char]
                      showLine = map showSquare
                      showSquare :: Piece p => Square p -> Char
                      showSquare (Just x) = showPiece x
                      showSquare Nothing = '.'
                                         
