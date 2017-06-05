module Board where

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

instance PieceReader ChessPiece where
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

instance BoardReader ChessPiece

class PieceReader p where
    readPiece :: Char -> Maybe p
    showPiece :: p -> Char

class (PieceReader p) => BoardReader p where
    readBoard :: String -> Maybe [[Maybe p]]
    readBoard = (mapM readLine) . lines
                where readLine :: (PieceReader p) => [Char] -> Maybe [Maybe p]
                      readLine = mapM readSquare
                      readSquare :: (PieceReader p) => Char -> Maybe (Maybe p)
                      readSquare '.' = Just Nothing
                      readSquare a = case (readPiece a) of
                                        Just b    -> Just (Just b)
                                        otherwise -> Nothing
                                         


