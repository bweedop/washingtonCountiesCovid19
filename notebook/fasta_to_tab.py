from Bio import SeqIO

path = "/Users/rachel/Desktop/ECOL8350Capstone/"
records = SeqIO.parse(path + "genbankSequences.noAmbiguous.mafftout.trimmed.fasta","fasta")
count = SeqIO.write(records,path +  "genbankSequences.noAmbiguous.mafftout.trimmed.tab", "tab")

print("Converted %i records" % count)