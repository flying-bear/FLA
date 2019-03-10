## sample of a file:
##    @UTF8
##    @PID:	11312/c-00028159-1
##    @Begin
##    @Languages:	fra
##    @Participants:	CHI Marie Target_Child , MOT Mother , FAT Father
##    @ID:	fra|Geneva|CHI|1;08.26||||Target_Child|||
##    @ID:	fra|Geneva|MOT||female|||Mother|||
##    @ID:	fra|Geneva|FAT||male|||Father|||
##    @Media:	010826, audio, unlinked
##    @Location:	at home , Geneva , CH
##    @Date:	11-NOV-1995
##    @Time Duration:	9:00-9:45
##    @Comment:	Marie prononce la plupart du temps [z] pour (j) et (ge) et [s] pour (ch)
##    @Comment:	MLU is 1.641
##    *FAT:	alors Marie (.) qu'est ce que tu veux faire ?
##    %mor:	adv|alors n:prop|Marie pro:rel|que$v:aux|être&PRES&3s pro:dem|ce
##            adv|que pro:subj|tu v:mdl|vouloir&PRES&12s v:mdllex|faire-INF ?
##    *FAT:	avec quoi tu joues là ?
##    %mor:	prep|avec pro:int|quoi part|taire-PP&m v|jouer-PRES&SUB&2s
##            adv:place|là ?
##    *CHI:	le cocodicoda@c .
##    %mor:	det:art|le&m&sg chi|cocodicoda .
##    %com:	nom d'une chanson; Marie chante des variations sur le mot .
##    *CHI:	non papa .
##    %mor:	co|non=no n|papa&m .
##    *FAT:	on fait le petit canard ?

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
    word_regex_dict = {'je': 'je|j\'|me|m\'|moi',
                       'tu': 'tu|t\'|te|toi',
                       'il': 'il|pro:obj\|le|pro:obj\|la|elle|se|s\'|soi',
                       'on': 'on', # se etc
                       'y': 'pro:y\|en|pro:y\|y', # not found even once!!
                       'nous': 'nous',
                       'vous': 'vous',
                       'ils': 'ils|elles|eux|pro:obj\|les'} #leur
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



if __name__ == "__main__":
    main()
