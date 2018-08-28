## code from https://github.com/Zapaan/python-chemical-formula-parser/blob/master/parser.py
## saved as atomparser

import re
from collections import Counter


ATOM_REGEX = '([A-Z][a-z]*)(\d*)'
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
            res[atom] += int(n or 1)
        except KeyError:
            res[atom] = int(n or 1)
    return res


def _fuse(mol1, mol2, w=1):
    """
    Fuse 2 dicts representing molecules. Return a new dict.

    This fusion does not follow the laws of physics.
    """
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
            m = re.match('\d+', formula[i+1:])
            if m:
                weight = int(m.group(0))
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

        i+=1

    # Fuse in all that's left at base level
    return _fuse(mol, _dictify(re.findall(ATOM_REGEX, ''.join(q)))), i


def parse_formula(formula):
    """Parse the formula and return a dict with occurences of each atom.

    Args:
       formula (str): the chemical formula 

    Returns:
       dict with occurences of each atom

    Example:
          >>> dict = parse_formula('H2O')
              print(dict)   
    """
    if not is_balanced(formula):
        raise ValueError("Watch your brackets ![{]$[&?)]}!]")

    return _parse(formula)[0]
