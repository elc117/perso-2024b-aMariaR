Jogo da Velha coitado

Meu tema escolhido como produção individual foi desenvolver um jogo da velha em haskell.
A ideia que implementei é bem simples, com os cliques do mouse uma posição é selecionada e um círculo aparece então na posição.
Os primeiros passos do projeto foram criar uma "estrutura", uma ideia de como o jogo funciona, que saiu assim:
1° versão:

-Definição da estrutura do jogo: como deve ocorrer
--Lista de posições
--  1  2  3
--  4  5  6
--  7  8  9

--abre tela de instrucao
--(se quer x ou o, "para sair digite...""digite o numero da posicao que quer jogar")
--pessoa digitou = jogo comeca
--desenha a grade com os numeros das posicoes ao fundo
--cria lista de tuplas ([char],Int) -> cada posição contém um x e o e o numero da posicao
--le o teclado com uma posicao dada por 1 2 3 4 5 6 7 8 9
--atualiza tela com x ou o na posicao dada
--verifica a combinacao
--sorteia uma posicao
--atualiza a tela com x ou y na posicao sorteada
--verifica a combinacao
--segue ate todas as posicoes serem preenchidas ou alguem ganhar
--inicia novo jogo

Porém, durante a construção do código os pesadelos começaram a surgir, a parte do back end era simples, poucas funções:
2° mudança de logica para ter apenas uma lista de interios

3° uma lista para o usario e uma para o sorteio, listas de bool

4° apenas lista de inteiros 1 equivale ao jogador e 2 ao computador
primeiras funções:

recebePosicao :: [Int] -> IO[Int]
recebePosicao pos = x <- readLine
if pos !! x == 0
then return take x pos ++ [1] drop (x +1) pos
else recebePosicao pos


sorteiaPosicao:: [Int] -> [Int]
sorteiaPosicao pos = x <- random(1, 9)
if pos !! x == 0
then return take x pos ++ [2] drop (x +1) pos
else verificaPosicao pos

Estava simples e acessível até o momento e a construção seguiu:

5° atualizacao

import Graphics.Gloss
import System.Exit (exitSuccess, exitFailure)


recebePosicao :: [Int] -> IO[Int]
recebePosicao pos = x <- readLine
if pos !! x == 0
then return take x pos ++ [1] drop (x +1) pos
else recebePosicao pos


sorteiaPosicao:: [Int] -> [Int]
sorteiaPosicao pos = x <- random(1, 9)
if pos !! x == 0
then return take x pos ++ [2] drop (x +1) pos
else verificaPosicao pos

verificaCombinacao:: [Int] -> [Int]
verificaCombinacao [a, b, c, d, e, f, g, h, i] = 
| (a == b && b == c)  || (d == e && e == f)  || (g == h && h == i)   
|| (g == h && h == i) || (a == d && d == g)  || (b == e && e == h) 
|| (c == f && f == i) || (a == e && e == i)  || (c == e && e == g)
= ganhouJogo [a, b, c, d, e, f, g, h, i]
| not(elem 0 pos) = encerraJogo ()
| otherwise = [a, b, c, d, e, f, g, h, i]

ganhouJogo :: [Int]
ganhouJogo pos = if x > y then desenhaJogadorGanhou else desenhaPerdeu
where x <- filter (\z <- z == 1) pos
      y <- filter (\z <- z == 2) pos

encerraJogo :: 
encerraJogo = exitSuccess


main :: IO ()
main = do
    pos <- [0,0,0,0,0,0,0,0,0]
    --funcoes de desenho
    recebePosicao pos
    --desenho
    verificaCombinacao pos
    sorteiaPosicao pos
    --desenho
    verificaCombinacao pos

6° atualizacao
Aqui o belo mundo desandou, admito que me perdi com o gloss, não entedia os exemplos, como a picture que era um estado da tela, então eu o gpt começamos nossa jornada juntos, ele me explicava e eu não entendia, as coisas ficaram sombrias, descobri que existia dados estruturados em haskell, o que foi um choque. Passei a analisar códigos de jogo da velha disponíveis e entendia menos ainda, as ferramentas e técnicas de codificação eram muito complexas para mim. Eu sequer sabia fazer um laço em haskell e descobri que para o jogo ocorrer existia um play na biblioteca gloss para atualizar a tela. Então minhas lindas funções eram inúteis, nada funcionava. 

Onde tudo estava perdido, tive que abrir a cabeça para aceitar que a minha estrutura definida e meu plano de jogo não ia dar certo da maneira como quis.
A entrada do jogo passou a ser mouse, pois o gloss e entrada do teclado não queriam funcionar juntos, aí algo apareceu na tela, um sucesso. Mas a grade do jogo da velha era tenebrosa, linhas e colunas fora do lugar, uma curta e outra mais longa, depois de muitos ctrl s, cabal build e cabal run, consegui ajustar. Mas aí a lógica de entrada não estava certa, uma hora eu ganhava mas aparecia o computador.

Mas no fim deu certo, em partes, minhas grandes considerações são:

-Ver haskell e programar em haskell é outra história, para programar funcional eu precisava tirar imperativo da minha cabeça e isso foi muito difícil;

-Descobri mais uma vez que front é um caos;

-Sofri para configurar o ambiente, sem dúvidas a lingaguem mais dificil de configurar que eu tive contato, instalar dependências, configurar o .cabal, 
aqui foi quase um dia;

-Haskell não encontrei muita documentação, como eu disse, os exemplos que eu tinha encontrado eram super complexos e eu não entendia nada;

-Porém descobri recursos novos, muitos deles, nem sei se processei todos;

-Entendi o que de fato é um outro paradigma e que o que não muda não é só a sintaxe, mas a forma que tu tem que pensar;

-A partir do 6° eu abandonei as atualizações, perdi a paciência com meu diário de construção;

-Mas de fato para um jogo funcionar o código é muito mais curto que em C, e mais limpo.

Admito que usei o gpt mas sem ele não teria condições, sem dúvidas eu me planejei mal no tempo, subestimei absurdamente o trabalho achando que seria simples, não tinha aprendido a lição. Fiquei muito tempo no laboratório também, estamos quase na véspera de competição (sim, inclusive faltarei uma semana interia novamente em novembro) então estamos lutando pra conseguir testar tudo, pois nada funciona, então fiquei num ponto que ou eu usava ia para ajudar, ou não entregava, prefiri assim, obviamente que o aproveitamento foi menor e eu estou considerando isto na nota final pois foi minha escolha.
Para além disso agradeço a voce, professora, por não ter largado o trabalho nos nossos pés sem sequer dar direção (acontece muito). Um abraço!
*O código está na pasta.

