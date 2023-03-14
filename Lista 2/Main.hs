module Biblioteca where

-- Importa a função nub do módulo Data.list para remover duplicatas de uma lista
import Data.List (nub)

-- Definição de tipos
type Pessoa = String
type Livro = String
type Emprestado = Bool 
type BancoDados = [(Pessoa, Livro, Emprestado)]

-- Exemplo de Banco de Dados (exemploBD)
exemploBD :: BancoDados
exemploBD = [("Leandro","Java",True),("Joabe", "CSP", True),("Lucas", "UML", True), ( "Lucas" , "Haskell", True),( "Sidney" , "CSP", True),( " " , "Java", False),( " " , "Concorrencia", False)]

-- Retorna uma lista com os títulos de todos os livros presentes no banco de dados
livros :: BancoDados -> [Livro]
livros bd = nub [l | (_,l,_) <- bd]

-- Retorna True se o livro 1 está disponível para empréstimo
livroDisponivel :: BancoDados -> Livro -> Bool 
livroDisponivel bd livro = not $ any (\(_, l, e) -> l == livro && e) bd

-- Retorna uma lista com os títulos dos livros emprestados para a pessoa especificada
livrosPessoa :: BancoDados -> Pessoa -> [Livro] 
livrosPessoa bd pessoa = [l | (p, l, _) <- bd, p == pessoa]

-- Retorna uma lista com os pares (livro, pessoa) para cada empréstimo presente no banco de dados
emprestimos :: BancoDados -> [(Livro,Pessoa)] 
emprestimos bd = [(l,p) | (p, l, e) <- bd, e]

-- Adiciona um novo empréstimo ao banco de dados, atualizado o campo “emprestado” para True (verdade)
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados 
emprestar bd pessoa livro
  | livroDisponivel bd livro = (pessoa, livro, True) : bd
  | otherwise = bd

-- Remove um empréstimo do banco de dados, atualizado o campo “emprestado” para False (falso)
devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd pessoa livro = [(p, l, False) | (p, l, True) <- bd, p /= pessoa || l /= livro] ++ [(pessoa, livro, False) | livroDisponivel bd livro]

-- Função principal para teste das funções:
main = do
  print $ livros exemploBD -- ["Java","CSP","UML","Haskell","Concorrencia"]
  print $ livroDisponivel exemploBD "Java" -- False
  print $ livroDisponivel exemploBD "Python" -- True
  print $ livrosPessoa exemploBD "Lucas" -- ["UML","Haskell"]
  print $ emprestimos exemploBD -- [("Java","Leandro"),("CSP","Joabe"),("UML","Lucas"),("Haskell","Lucas"),("CSP","Sidney")]
  let bd1 = emprestar exemploBD "Fulano" "Java"
  print bd1 -- [("Fulano","Java",True),("Leandro","Java",True),("Joabe","CSP",True),("Lucas","UML",True),("Lucas","Haskell",True),("Sidney","CSP",True),("","Concorrencia",False)]
  let bd2 = devolver bd1 "Fulano" "Java"
  print bd2 -- [("Leandro","Java",True),("Joabe","CSP",True),("Lucas","UML",True),("Lucas","Haskell",True),("Sidney","CSP",True),("","Concorrencia",False)]