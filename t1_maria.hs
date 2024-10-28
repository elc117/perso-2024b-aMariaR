module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (isNothing)

data Jogo = Jogo
  { posicoes :: [Int]  
  , jogadorAtual :: Int
  , status :: Status    
  }

type Status = String

statusEmAndamento :: Status
statusEmAndamento = "EmAndamento"

statusVitoriaJogador :: Status
statusVitoriaJogador = "VitoriaJogador"

statusVitoriaComputador :: Status
statusVitoriaComputador = "VitoriaComputador"

statusEmpate :: Status
statusEmpate = "Empate"

janela :: Display
janela = InWindow "Jogo da Velha" (600, 600) (100, 100)

fundo :: Color
fundo = light green

main :: IO ()
main = do
    let posicoesIniciais = replicate 9 0
    let jogoInicial = Jogo posicoesIniciais 1 statusEmAndamento
    play janela fundo 30 jogoInicial desenhaJogo trataEvento atualizaJogo

desenhaJogo :: Jogo -> Picture
desenhaJogo (Jogo posicoes _ status) = pictures (desenhaGrade ++ desenhaSimbolos posicoes ++ [mensagemStatus status])

desenhaGrade :: [Picture]
desenhaGrade =
    [ line [(-250, 30), (250, 30)]
    , line [(-250, -180), (250, -180)]
    , line [(-90, 200), (-90, -300)]
    , line [(120, 200), (120, -300)]
    ]

desenhaSimbolos :: [Int] -> [Picture]
desenhaSimbolos posicoes = [desenhaSimbolo (posicoes !! i) i | i <- [0..8]]

desenhaSimbolo :: Int -> Int -> Picture
desenhaSimbolo 0 _ = blank
desenhaSimbolo jogador i = translate x y simbolo
  where
    (x, y) = posicaoParaCoordenadas i
    simbolo = if jogador == 1
              then color blue (thickCircle 60 30)
              else color red (circleSolid 60)

mensagemStatus :: Status -> Picture
mensagemStatus status
  | status == statusEmAndamento = blank
  | status == statusVitoriaJogador = translate (-150) 250 $ scale 0.3 0.3 $ color blue $ text "Voce venceu!"
  | status == statusVitoriaComputador = translate (-200) 250 $ scale 0.3 0.3 $ color red $ text "Computador venceu!"
  | status == statusEmpate = translate (-100) 250 $ scale 0.3 0.3 $ color black $ text "Empate!"
  | otherwise = blank

posicaoParaCoordenadas :: Int -> (Float, Float)
posicaoParaCoordenadas i = (x, y)
  where
    x = fromIntegral ((i `mod` 3) * 200 - 200)
    y = fromIntegral (100 - (i `div` 3) * 200)

trataEvento :: Event -> Jogo -> Jogo
trataEvento (EventKey (MouseButton LeftButton) Up _ (mx, my)) jogo@(Jogo posicoes jogador status)
  | status == statusEmAndamento && posicoes !! pos == 0 =
      let novasPosicoes = take pos posicoes ++ [jogador] ++ drop (pos + 1) posicoes
          novoStatus = verificaVitoria novasPosicoes
      in Jogo novasPosicoes (if novoStatus == statusEmAndamento then 3 - jogador else jogador) novoStatus
  where pos = posicaoClique (mx, my)
trataEvento _ jogo = jogo

atualizaJogo :: Float -> Jogo -> Jogo
atualizaJogo _ jogo
  | status jogo == statusEmAndamento && jogadorAtual jogo == 2 =
      let posComputador = escolhePosicaoComputador (posicoes jogo)
          novasPosicoes = take posComputador (posicoes jogo) ++ [2] ++ drop (posComputador + 1) (posicoes jogo)
          novoStatus = verificaVitoria novasPosicoes
      in Jogo novasPosicoes 1 novoStatus
atualizaJogo _ jogo = jogo

posicaoClique :: (Float, Float) -> Int
posicaoClique (x, y) =
    let coluna = floor ((x + 300) / 200)
        linha = 2 - floor ((y + 300) / 200)
    in linha * 3 + coluna

escolhePosicaoComputador :: [Int] -> Int
escolhePosicaoComputador posicoes = head [i | i <- [0..8], posicoes !! i == 0]

verificaVitoria :: [Int] -> Status
verificaVitoria posicoes =
    let vitoriaCombinacoes = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]]
        checaVitoria [a, b, c] =
            if posicoes !! a == posicoes !! b && posicoes !! b == posicoes !! c && posicoes !! a /= 0
            then Just (posicoes !! a)
            else Nothing
        vencedores = filter (/= Nothing) $ map checaVitoria vitoriaCombinacoes
        primeiroVencedor = if null vencedores then Nothing else head vencedores
    in if isNothing primeiroVencedor
       then if notElem 0 posicoes then statusEmpate else statusEmAndamento
       else case primeiroVencedor of
            Just 1 -> statusVitoriaJogador
            Just 2 -> statusVitoriaComputador
            _ -> statusEmAndamento
