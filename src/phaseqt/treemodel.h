/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/treemodel.h */
/*  Date      : <23 Nov 11 09:31:41 flechsig>  */
/*  Time-stamp: <24 Nov 11 12:57:07 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

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

#ifndef TREEMODEL_H
#define TREEMODEL_H

#include <QAbstractItemModel>
#include <QModelIndex>
#include <QVariant>

#include "phaseqt.h"


class TreeItem;
class QLineEdit;
class QListWidget;

class TreeModel : public QAbstractItemModel
{
    Q_OBJECT

public:
  TreeModel(const QString &data, QObject *parent = 0, QLineEdit *eddi = 0, QListWidget *plist = 0);
    ~TreeModel();

    QVariant data(const QModelIndex &index, int role) const;
    Qt::ItemFlags flags(const QModelIndex &index) const;
    QVariant headerData(int section, Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const;
    QModelIndex index(int row, int column,
                      const QModelIndex &parent = QModelIndex()) const;
    QModelIndex parent(const QModelIndex &index) const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    int columnCount(const QModelIndex &parent = QModelIndex()) const;
    int getActualIndex();
    void updateItemVal(QString, int);

private:
    void setupModelData(const QStringList &lines, TreeItem *parent);

    TreeItem   *rootItem;
    TreeItem* itemList[NPARS];
    int actualIndex;
    QLineEdit *myeddi;
    QListWidget *myplist;
    

private slots:
    void selectSlot(const QModelIndex &);
};

#endif
