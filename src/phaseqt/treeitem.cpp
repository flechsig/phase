//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/treeitem.cpp
//  Date      : <23 Nov 11 09:41:11 flechsig> 
//  Time-stamp: <24 Nov 11 15:19:53 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

/****************************************************************************
**
** Copyright (C) 2005-2006 Trolltech ASA. All rights reserved.
**
** This file is part of the example classes of the Qt Toolkit.
**
** This file may be used under the terms of the GNU General Public
** License version 2.0 as published by the Free Software Foundation
** and appearing in the file LICENSE.GPL included in the packaging of
** this file.  Please review the following information to ensure GNU
** General Public Licensing requirements will be met:
** http://www.trolltech.com/products/qt/opensource.html
**
** If you are unsure which license is appropriate for your use, please
** review the following information:
** http://www.trolltech.com/products/qt/licensing.html or contact the
** sales department at sales@trolltech.com.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

/*
    treeitem.cpp

    A container for items of data supplied by the simple tree model.
*/

#include <QStringList>
#include <iostream>

#include "treeitem.h"

using namespace std;

TreeItem::TreeItem(const QList<QVariant> &data, TreeItem *parent)
{
    parentItem = parent;
    itemData = data;
}

TreeItem::~TreeItem()
{
    qDeleteAll(childItems);
}

void TreeItem::appendChild(TreeItem *item)
{
    childItems.append(item);
}

TreeItem *TreeItem::child(int row)
{
    return childItems.value(row);
}

int TreeItem::childCount() const
{
    return childItems.count();
}

int TreeItem::columnCount() const
{
    return itemData.count();
}

QVariant TreeItem::data(int column) const
{
    return itemData.value(column);
}

TreeItem *TreeItem::parent()
{
    return parentItem;
}

int TreeItem::row() const
{
    if (parentItem)
        return parentItem->childItems.indexOf(const_cast<TreeItem*>(this));

    return 0;
}

// some special functions for our purpose
// set value
void TreeItem::setValue(QString *val) 
{
  //  cout << __FILE__ << " setValue called, val= " << val->toLocal8Bit().constData() << endl;
  
  if (columnCount() < 2)
    return;

  //  cout << __FILE__ << " do the set" << endl;
  
  QString a= QString(*val);
  itemData.replace(1, a);
  
  /*
  cout << __FILE__ << " set done " << endl;*/
  //cout << childCount() << endl;
  //  QVariant a= data(0); 
  // QStringList b= a.toStringList();
  // for (int i = 0; i < b.size(); ++i)
  //   cout << i << " putval data: " << b.at(i).toLocal8Bit().constData() << endl;

  
} // setVal

// get index
// returns the index or -1 in case of an error
int TreeItem::getIndex()
{
#ifdef DEBUG1
  cout << "getIndex called " << endl;
#endif

  if (columnCount() < 5)
    return -1;

  return data(4).toInt();
} // getIndex

// get value
// returns the value or -1 in case of an error
QString TreeItem::getValue()
{
#ifdef DEBUG1
  cout << "getValue called " << endl;
#endif

  if (columnCount() < 2)
    return QString("error in getValue");

  return data(1).toString();
} // getValue
