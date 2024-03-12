with open('./dados/dados_prenatal.csv', 'r') as infile, open('./dados/dados_prenatal_mod.csv', 'w') as outfile:
    for line in infile:
        # Remove the trailing ";"
        modified_line = line.rstrip(';\n')
        outfile.write(modified_line + '\n')