# File      : /afs/psi.ch/project/phase/GIT/phase/src/phasepython/atomparser.py
# Date      : <29 Aug 18 10:02:57 flechsig> 
# Time-stamp: <2023-03-01 11:07:19 flechsig> 
# Author    : Flechsig Uwe, uwe.flechsig&#64;psi.&#99;&#104;

"""atomparser module"""



## code from https://github.com/Zapaan/python-chemical-formula-parser/blob/master/parser.py
## saved as atomparser
## UF Aug 2018 add floating weights functionality

import re
from collections import Counter


# UF ATOM_REGEX = '([A-Z][a-z]*)(\d*)'    
ATOM_REGEX = '([A-Z][a-z]*)(\d*\.*\d*)'    # UF add \.*\d*
OPENERS = '({['
CLOSERS = ')}]'


def is_balanced(formula):
    """Check if all sort of brackets come in pairs."""
    # Very naive check, just here because you always need some input checking
    c = Counter(formula)
    return c['['] == c[']'] and c['{'] == c['}'] and c['('] == c[')']


def _dictify(tuples):
    """Transform tuples of tuples to a dict of atoms."""
    res = dict()
    for atom, n in tuples:
        try:
            # UF res[atom] += int(n or 1)
            res[atom] += float(n or 1)
            # print("debug _dictify UF try atom={}, n={}, tuples={}".format(atom,n,tuples))
        except KeyError:
            # UFres[atom] = int(n or 1)
            # print("debug _dictify UF keyerror, atom={}, n={}, tuples={}".format(atom,n,tuples))
            res[atom] = float(n or 1)
    return res


def _fuse(mol1, mol2, w=1):
    """
    Fuse 2 dicts representing molecules. Return a new dict.

    This fusion does not follow the laws of physics.
    """
    # print("UF debug call fuse mol1={}, mol2={}, w={}".format(mol1, mol2, w))
    return {atom: (mol1.get(atom, 0) + mol2.get(atom, 0)) * w for atom in set(mol1) | set(mol2)}


def _parse(formula):
    """
    Return the molecule dict and length of parsed part.

    Recurse on opening brackets to parse the subpart and
    return on closing ones because it is the end of said subpart.
    """
    q = []
    mol = {}
    i = 0

    while i < len(formula):
        # Using a classic loop allow for manipulating the cursor
        token = formula[i]

        if token in CLOSERS:
            # Check for an index for this part
            # UF m = re.match('\d+', formula[i+1:])
            m = re.match('\d+\.*\d*', formula[i+1:])  # UF match also float
            if m:
                # UF weight = int(m.group(0))
                weight = float(m.group(0))
                # print("UF debug weight: {}".format(weight))
                i += len(m.group(0))
            else:
                weight = 1

            submol = _dictify(re.findall(ATOM_REGEX, ''.join(q)))
            return _fuse(mol, submol, weight), i

        elif token in OPENERS:
            submol, l = _parse(formula[i+1:])
            mol = _fuse(mol, submol)
            # skip the already read submol
            i += l + 1
        else:
            q.append(token)
            # print("uf debug append ", q)

        i += 1
        # heinz= re.findall(ATOM_REGEX, ''.join(q))
        # print("UF debug heinz={}, i= {}".format(heinz, i))

    # Fuse in all that's left at base level
    return _fuse(mol, _dictify(re.findall(ATOM_REGEX, ''.join(q)))), i
# end _parse


def parse_formula(formula):
    """Parse the formula and return a dict with occurences of each atom.

    Args:
       formula (str): the chemical formula 

    Returns:
       dict: dictionary with occurences of each atom

    Example:
          >>> dict = parse_formula('H2O')
              print(dict)  
 
    """

    if not is_balanced(formula):
        raise ValueError("Watch your brackets ![{]$[&?)]}!]")

    return _parse(formula)[0]
