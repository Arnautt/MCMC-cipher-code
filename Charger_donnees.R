## Pour charger les données ##

# Matrice de transition M
txt_as_ref = readLines("warandpeace.txt")              # Lecture d'un texte de référence
txt_as_ref = toupper(txt_as_ref)                       # Convertit chaque lettre en majuscule
M = matrix(0,27,27)                                    # Matrice de transition d'une lettre à une autre (bigrammes)
alphabet <- toupper(letters)                           # Definition de l'alphabet et alphabet avec espace (27 caractères)
alphabet_with_space = c(alphabet, " ")                  
rownames(M) = alphabet_with_space
colnames(M) = alphabet_with_space 
old_char = " "
'%!in%' <- function(x,y)!('%in%'(x,y))



for (ligne in 1:length(txt_as_ref)) {                                          # Parcourt les lignes du texte
  my_line <- stringi::stri_trans_general(txt_as_ref[ligne], "latin-ascii")     # 2 lignes : enlève les accents et la ponctuation
  my_line <- gsub("[[:punct:]]", "", toupper(as.character(my_line)))
  for (pos in 1:nchar(my_line)) {                                             # Parcourt les lettres de la ligne
    new_char = substring(my_line,pos,pos)                                     # On extrait la lettre
    if (new_char %!in% alphabet_with_space) {}                                # si caractère inconnu, on ne fait rien
    else if ((new_char == " " & old_char == " ")) {}                          # si deux espaces consécutifs, on ne fait rien
    else {
      x <- match(old_char, alphabet_with_space)
      y <- match(new_char, alphabet_with_space)
      M[x,y] = M[x,y] + 1                                                     # sinon, on ajoute 1 à la proba de passer de x à y
      old_char = new_char
    }
  }
  new_char = " "                                                              # fin de ligne => caractère = espace
  if (old_char %in% alphabet) {
    x <- match(old_char, alphabet_with_space)
    M[x,27] = M[x,27]+1
  }
  old_char=" "
}


M_trans <- M+1                                              # On divise par une constante pour avoir une proba
sum_s <- rowSums(M_trans)
for (i in 1:nrow(M_trans)) {
  M_trans[i,] <- M_trans[i,] / sum_s[i]
}

save(M_trans, file="M_trans.Rdata")


# Exemple chiffrage


t1 <- "EXEMPLE CHIFFRAGE"
rdm_code <- initialization()
to_decode <- decoder(rdm_code, t1)
t2 <- c()
for (i in 1:26) {
  t2 <- c(t2, rdm_code[[i]])
}
t2 <- paste(t2, collapse="")
t3 <- to_decode
t4 <- c()
for (i in 1:26) {
  t4 <- c(t4, antecedent(rdm_code, LETTERS[i]))
}
t4 <- paste(t4, collapse="")
t5 <- t1
Exemple_chiffrage <- c(t1,t2,t3,t4,t5)
df <- as.matrix(Exemple_chiffrage)
rownames(df) <- c("Texte : ", 'Code : ', "Crypté : ", "Clé : ", "Décrypté : ")
colnames(df) <- c("")
df <- as.table(df)
df

save(df, file="txt.txt")
