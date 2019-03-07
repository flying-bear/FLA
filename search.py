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
        data['lang'] = match_lang[0]
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
    match_child = re.findall('@Participants:.+?CHI (.+?) .+?\n',text)
    if match_child:
        data['name'] = match_child[0]
    match_participants = list(set(re.findall('(?:\t| )([A-Z]{3})', text)))
    if match_participants:
        data['participants'] = match_participants
    return data



def extract_words_by_participant(text, words, participants): # count how many times each word in words was used by each participant
                                               # count the amount of utterances / words by each participant
    words_by_participant = dict(zip(participants, [dict(zip(words, [0 for j in range(len(words))])) for i in range(len(participants))]))
    number_of_words_by_participant = dict(zip(participants, [0 for i in range(len(participants))]))
    lines = text.split('\n')
    for line in lines:
        line = re.sub('[\!"#\$%&\\\(\)\+,-\.\/:;<=>\?@\[\]\^_`{\|}~]','',line) # strip punctuation
        line = re.sub('\'(?:ll|d|s)','',line)
        for p in participants:
            if re.match(f'\*{p}', line):
                line = line.lower()
                all_line_words = line.split('\t')[1].split()
                number_of_words_by_participant[p] += len(all_line_words)
                counted_line_words = Counter(all_line_words)
                for word in words:
                    if word in all_line_words:
                            words_by_participant[p][word] += counted_line_words[word]
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
    words = ['je', 'j\'','me', 'm\'', 'moi',
             'tu', 'te', 't\'', 'toi'
             'il', 'elle', 'lui', 'en', 'se', 's\'', 'soi', # la, le, \'l
             'on', 'nous', # y
             'vous',
             'ils', 'elles', 'eux'] # leur, les
##    db = walk(input('print root directory: '), words)
##    with open('result.csv', 'w', encoding='utf-8') as file:
##        file.write('path,filename,languge,age,childname,participant,')
##        file.write(','.join(words)+'\n')
##        for key in db:
##            if not db[key]:
##                continue
##            for participant in db[key]['words_b_p']:
##                line = [key, db[key]['meta']['filename'], db[key]['meta']['lang'], str(db[key]['meta']['age']), db[key]['meta']['name']]
##                line.append(participant)
##                for word in words:
##                    line.append(str(db[key]['words_b_p'][participant][word]))
##                file.write(','.join(line)+'\n')
    filepath = '010826.cha'
    text = read_file(filepath)
    data = {'file': filepath}
    data.update(extract_meta(text))
    w_b_p = extract_words_by_participant(text, words, data['participants'])
    for p in w_b_p:
        print(p)
        for w in sorted(w_b_p[p]):
            print(w+': '+str(w_b_p[p][w]))


if __name__ == "__main__":
    main()
