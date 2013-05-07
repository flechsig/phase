//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/treemodel.cpp
//  Date      : <22 Nov 11 14:32:21 flechsig> 
//  Time-stamp: <07 May 13 14:25:32 flechsig> 
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
    treemodel.cpp

    Provides a simple tree model to show how to create and use hierarchical
    models.
    extensions for phaseqt by UF
*/

#include <QtGui>
#include <iostream>

#include "treeitem.h"
#include "treemodel.h"

using namespace std;

TreeModel::TreeModel(const QString &data, QObject *parent, QLineEdit *eddi)
    : QAbstractItemModel(parent)
{
    QList<QVariant> rootData;
    rootData << "Variable" << "Value" << "Description" << "Default" << "Index";
    rootItem = new TreeItem(rootData);
    setupModelData(data.split(QString("\n")), rootItem);
    myeddi= eddi;
}

TreeModel::~TreeModel()
{
    delete rootItem;
}

int TreeModel::columnCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return static_cast<TreeItem*>(parent.internalPointer())->columnCount();
    else
        return rootItem->columnCount();
}

QVariant TreeModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    if (role != Qt::DisplayRole)
        return QVariant();

    TreeItem *item = static_cast<TreeItem*>(index.internalPointer());

    return item->data(index.column());
}

Qt::ItemFlags TreeModel::flags(const QModelIndex &index) const
{
    if (!index.isValid())
        return Qt::ItemIsEnabled;

    return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

QVariant TreeModel::headerData(int section, Qt::Orientation orientation,
                               int role) const
{
    if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
        return rootItem->data(section);

    return QVariant();
}

QModelIndex TreeModel::index(int row, int column, const QModelIndex &parent)
            const
{
    TreeItem *parentItem;

    if (!parent.isValid())
        parentItem = rootItem;
    else
        parentItem = static_cast<TreeItem*>(parent.internalPointer());

    TreeItem *childItem = parentItem->child(row);
    if (childItem)
        return createIndex(row, column, childItem);
    else
        return QModelIndex();
}

QModelIndex TreeModel::parent(const QModelIndex &index) const
{
    if (!index.isValid())
        return QModelIndex();

    TreeItem *childItem = static_cast<TreeItem*>(index.internalPointer());
    TreeItem *parentItem = childItem->parent();

    if (parentItem == rootItem)
        return QModelIndex();

    return createIndex(parentItem->row(), 0, parentItem);
}

int TreeModel::rowCount(const QModelIndex &parent) const
{
    TreeItem *parentItem;

    if (!parent.isValid())
        parentItem = rootItem;
    else
        parentItem = static_cast<TreeItem*>(parent.internalPointer());

    return parentItem->childCount();
}

// read data from the configuration file
void TreeModel::setupModelData(const QStringList &lines, TreeItem *parent)
{
  QList<TreeItem*> parents;
  QList<int> indentations;
  parents << parent;
  indentations << 0;
  
  int number = 0;
  
  while (number < lines.count()) 
    {
      // cout << number << "comment" << endl;
      int position = 0;
      while (position < lines[number].length()) 
	{
	  if (lines[number].mid(position, 1) != " ")
	    break;
	  position++;
	}
      
      QString lineData = lines[number].mid(position).trimmed();
#ifdef DEBUG1
      cout << number << " -> " << lineData.toLocal8Bit().constData() << endl;
#endif      
      if (*lineData.toLocal8Bit().constData() != '#') // comment
	{
	  if (!lineData.isEmpty()) 
	    {
	      // Read the column data from the rest of the line.
	      QStringList columnStrings = lineData.split("\t", QString::SkipEmptyParts);
	      QList<QVariant> columnData;
	      for (int column = 0; column < columnStrings.count(); ++column)
		columnData << columnStrings[column];
	      
	      if (position > indentations.last()) 
		{
		  // The last child of the current parent is now the new parent
		  // unless the current parent has no children.
		  
		  if (parents.last()->childCount() > 0) 
		    {
		      parents << parents.last()->child(parents.last()->childCount()-1);
		      indentations << position;
		    }
		} 
	      else 
		{
		  while (position < indentations.last() && parents.count() > 0) 
		    {
		      parents.pop_back();
		      indentations.pop_back();
		    }
		} // end else position > indentations.last()
	      
	      // Append a new item to the current parent's list of children.
	      TreeItem *myItem= new TreeItem(columnData, parents.last());
	      parents.last()->appendChild(myItem);
	      
	      int myidx= myItem->getIndex();
	      //   QString myval= myItem->getValue();  // for debugging
	      //   cout << "setupModelData - the index: " << myidx ;
	      //   cout << " val " <<  myval.toLocal8Bit().constData() << endl;
	      
	      if (myidx > -1)
		itemList[myidx]= myItem;  // save item pointer in linear pointer list
	      
	    } // end line not empty
	} // end if no comment
      number++;
    } // end while
} // setupModelData()

// the slot called if an item is selected
void TreeModel::selectSlot(const QModelIndex &index)
{
#ifdef DEBUG
  cout << "degug: " << __FILE__ << " selectSlot called "  << endl;
#endif

  if (!index.isValid()) return;

  QPersistentModelIndex valindex= QPersistentModelIndex(index.sibling(index.row(), 1));
  QPersistentModelIndex idxindex= QPersistentModelIndex(index.sibling(index.row(), 4));

  if (!valindex.isValid()) return;
  if (!idxindex.isValid()) return;

  int myidx=  data(idxindex, Qt::DisplayRole).toInt();
  actualIndex= myidx;                            // stores the index of the linear list
 
#ifdef DEBUG 
  cout  << "debug: " << __FILE__ << " index: " << myidx  << 
    ", value: " << data(valindex, Qt::DisplayRole).toString().toLocal8Bit().constData() << endl;
#endif
  
  QVariant    a= data(idxindex, Qt::DisplayRole);
  QStringList b= a.toStringList();

#ifdef DEBUG1
  cout << "debug: " << __FILE__ << " row, column " << index.row() << "," << index.column() << endl;
  cout << "stringlist: "<< endl;
  for (int i = 0; i < b.size(); ++i)
    cout << i << " selectSlot data: " << b.at(i).toLocal8Bit().constData() << endl;
#endif

  myeddi->setText(data(valindex, Qt::DisplayRole).toString()); // updates the edit widget
} // end selectSlot

// change the value of an item with index idx
void TreeModel::updateItemVal(QString val, int idx)
{
#ifdef DEBUG1  
  cout << __FILE__ << ": updateItemVal called, idx: " << idx << " val: " << val.toLocal8Bit().constData() << endl;
#endif

  TreeItem *myitem=  (TreeItem *)itemList[idx];   // get item from indexlist
  
  if (!myitem)                // important to test if item defined
    {
      cout << __FILE__ << " error: parameter No: " << idx << " skip it" << endl;
      return;
    }

  QString a= QString(val);  // copy constructor
  myitem->setValue(&a);
  emit layoutChanged();      // required to force an update of the widget
} // end updateItemLal

// exports the index since its private data
int TreeModel::getActualIndex()
{
  return actualIndex;
} // end getActualIndex
