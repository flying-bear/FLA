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


def read_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as file:
        return file.read()

def extract_meta(text):
    data = {'file': '',
            'lang': '',
            'age': -1,
            'name': ''}
    match_lang = re.findall('@Languages:\t(.+)\n', text)
    if match_lang:
        data['lang'] = match_lang[0]
    match_age = re.findall('@ID:\t.+\|.+\|CHI\|([0-9,.;]+).+\n', text)
    if match_age:
        y,r = match_age[0].split(';')
        m,d = r.split('.')
        data['age'] = 12*int(y) + int(m) + 1/30 * int(d)
    match_child = re.findall('@Participants:\tCHI (.+?) .+\n',text)
    if match_child:
        data['name'] = match_child[0]
    return data



def extract_words_by_participant(text):
    pass


def main():
    text = read_file('020409.cha')
    print(extract_meta(text))


if __name__ == "__main__":
    main()
