## sample of a file:
##  @UTF8
##  @PID:	11312/c-00018977-1
##  @Begin
##  @Languages:	eng
##  @Participants:	CHI Barbara Target_Child , MOT Ruth Mother , INV Cathy Investigator
##  @ID:	eng|Belfast|CHI|2;04.09|female||MC|Target_Child|||
##  @ID:	eng|Belfast|MOT||female|||Mother|||
##  @ID:	eng|Belfast|INV|||||Investigator|||
##  *MOT:	all these toys and everything !
##  %mor:	qn|all pro:dem|these n|toy-PL coord|and pro:indef|everything !
##  %gra:	1|3|QUANT 2|3|DET 3|0|INCROOT 4|3|CONJ 5|4|COORD 6|3|PUNCT
##  *INV:	and look at all these videos (.) too .
##  %mor:	coord|and cop|look prep|at qn|all pro:dem|these n|video-PL post|too
##          .
##  %gra:	1|2|LINK 2|0|ROOT 3|2|JCT 4|6|QUANT 5|6|DET 6|3|POBJ 7|6|PQ 8|2|PUNCT
##  *CHI:	put it down a wee bit .
##  %mor:	v|put&ZERO pro:per|it adv|down det:art|a n|wee n|bit .
##  %gra:	1|0|ROOT 2|1|OBJ 3|1|JCT 4|6|DET 5|6|MOD 6|1|JCT 7|1|PUNCT
##  @End

import os
import re
from collections import Counter
import matplotlib as plt


def read_file(filepath): # read file text
    with open(filepath, 'r', encoding='utf-8') as file:
        return file.read()

def extract_meta(text): # extract age, name and lang of the child
    data = {'lang': '',
            'age': -1,
            'name': '',
            'participants': {}}
    match_lang = re.findall('@Languages:\t(.+)\n', text)
    if match_lang:
        data['lang'] = re.sub(',', '&', match_lang[0])
    match_age = re.findall('@ID:\t.+\|.+\|CHI\|([0-9,.;]+).+\n', text)
    if match_age:
        y,r = match_age[0].split(';')
        if '.' in r:
            m,d = r.split('.')
            if d:
                data['age'] = 12*int(y) + int(m) + 1/30 * int(d)
            else:
                data['age'] = 12*int(y) + int(m)
        elif r:
            data['age'] = 12*int(y) + int(r)
        else:
            data['age'] = 12*int(y)
    match_child = re.findall('@Participants:.+?CHI (.+?)\s.*?\n',text)
    if match_child:
        data['name'] = match_child[0]
    match_participants = list(set(re.findall('\|([A-Z]{3})\|', text)))
    if match_participants:
        data['participants'] = match_participants
    return data

def clean_line(line):
    line = re.sub('[\!"#\$%&\\\(\)\+,\.\/:;<=>\?@\[\]\^_`{\|}~]','',line) # strip punctuation
    line = line.lower()
    return line


def extract_words_by_participant(text, word_regex_dict, participants): # count how many times each word in words was used by each participant
                                               # count the amount of utterances / words by each participant
    words_by_participant = dict(zip(participants, [dict(zip(word_regex_dict.keys(), [0 for j in range(len(word_regex_dict.keys()))])) for i in range(len(participants))]))
    number_of_words_by_participant = dict(zip(participants, [0 for i in range(len(participants))]))
    lines = text.split('\n')
    for i in range(len(lines)):
        line = lines[i]
        for p in participants:
            if re.match(f'\*{p}', line):
                line = clean_line(line)
                morph = lines[i+1]
                text = line + '\n' + morph
                all_line_words = line.split('\t')[1].split()
                number_of_words_by_participant[p] += len(all_line_words)
                for key in word_regex_dict:
                    match = re.findall(word_regex_dict[key], text)
                    if match:
                        words_by_participant[p][key] += len(match) 
    for p in participants:
        for word in words_by_participant[p]:
            if number_of_words_by_participant[p]:
                words_by_participant[p][word] = words_by_participant[p][word]/number_of_words_by_participant[p]
            else:
                words_by_participant[p][word] = 0
    return words_by_participant


def walk(folder, words): # walk a folder and extract frequencies of a given wordlist
    result_db = {}
    for address, dirs, files in os.walk(folder):
        for file in files:
            if len(file.split('.')) == 2:
                if not file.split('.')[1] == 'cha':
                    continue
                path = address+'\\'+file
                text = read_file(path)
                meta = extract_meta(text)
                if meta['age'] == -1:
                    continue
                meta.update({'filepath': path, 'filename': file})
                result_db[path] = {'meta': meta, 'words_b_p': extract_words_by_participant(text, words, meta['participants'])}
            else:
                continue
    return result_db
            

def main():
    word_regex_dict = {'1sg': 'pro:sub\|I|pro:obj\|me|myself',
                       '1pl': 'pro:sub\|we|pro:obj\|us|ourselves',
                       '2': 'you|yourself|yourselves',
                       '3sgM': 'pro:sub\|he|him|himself',
                       '3sgF': 'pro:sub\|she|pro:obj\|her|herself',
                       '3sgN': 'it|itself',
                       '3pl': 'they|them|themselves'}
    db = walk(input('print root directory: '), word_regex_dict)
    with open('result.csv', 'w', encoding='utf-8') as file:
        file.write('path,filename,languge,age,childname,participant,')
        file.write(','.join(word_regex_dict.keys())+'\n')
        for key in db:
            if not db[key]:
                continue
            for participant in db[key]['words_b_p']:
                line = [key, db[key]['meta']['filename'], db[key]['meta']['lang'], str(db[key]['meta']['age']), db[key]['meta']['name']]
                line.append(participant)
                for word in word_regex_dict.keys():
                    line.append(str(db[key]['words_b_p'][participant][word]))
                file.write(','.join(line)+'\n')
##    filepath = '010421.cha'
##    text = read_file(filepath)
##    data = {'file': filepath}
##    data.update(extract_meta(text))
##    w_b_p = extract_words_by_participant(text, ['i', 'me', 'you', 'she', 'he', 'it'], data['participants'])
##    for p in w_b_p:
##        print(p)
##        for w in sorted(w_b_p[p]):
##            print(w+': '+str(w_b_p[p][w]))


if __name__ == "__main__":
    main()
