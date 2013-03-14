#ifndef GDSDBREADER_H
#define GDSDBREADER_H

// This file contains core structures, classes and types for the entire gds app
// WARNING: DO NOT MODIFY UNTIL IT'S STRICTLY NECESSARY

#include <QDir>
#include "diagramwidget/qgldiagramwidget.h"

#define GDS_DIR "gdsdata"

enum level {LEVEL_ONE, LEVEL_TWO, LEVEL_THREE};

// The internal structure of the db to store information about each node (each level)
// this will be serialized before being written to file
class dbDataStructure
{
public:
    QString label;
    quint32 depth;
    quint32 userIndex;
    QByteArray data;    // This is COMPRESSED data, optimize ram and disk space, is decompressed
                        // just when needed (to display the comments)

    // The following ID is used to create second-third level files
    quint64 uniqueID;
    // All the next items linked to this one
    QVector<dbDataStructure*> nextItems;
    // Corresponding indices vector (used to store data)
    QVector<quint32> nextItemsIndices;
    // The father element (or NULL if it's root)
    dbDataStructure* father;
    // Corresponding indices vector (used to store data)
    quint32 fatherIndex;
    bool noFatherRoot; // Used to tell if this node is the root (so hasn't a father)

    // These fields will be useful for levels 2 and 3
    QString fileName; // Relative filename for the associated code file
    QByteArray firstLineData; // Compressed first line data, this will be used with the line number to retrieve info
    QVector<quint32> linesNumbers; // First and next lines (next are relative to the first) numbers

    // -- Generic system data not to be stored on disk
    void *glPointer; // GL pointer

    // These operator overrides prevent the glPointer and other non-disk-necessary data serialization
    friend QDataStream& operator<<(QDataStream& stream, const dbDataStructure& myclass)
    // Notice: this function has to be "friend" because it cannot be a member function, member functions
    // have an additional parameter "this" which isn't in the argument list of an operator overload. A friend
    // function has full access to private data of the class without having the "this" argument
    {
        // Don't write glPointer and every pointer-dependent structure
        return stream << myclass.label << myclass.depth << myclass.userIndex << qCompress(myclass.data)
                         << myclass.uniqueID << myclass.nextItemsIndices << myclass.fatherIndex << myclass.noFatherRoot
                            << myclass.fileName << qCompress(myclass.firstLineData) << myclass.linesNumbers;
    }
    friend QDataStream& operator>>(QDataStream& stream, dbDataStructure& myclass)
    {
        //Don't read it, either
        stream >> myclass.label >> myclass.depth >> myclass.userIndex >> myclass.data
                      >> myclass.uniqueID >> myclass.nextItemsIndices >> myclass.fatherIndex >> myclass.noFatherRoot
                         >> myclass.fileName >> myclass.firstLineData >> myclass.linesNumbers;
        myclass.data = qUncompress(myclass.data);
        myclass.firstLineData = qUncompress(myclass.firstLineData);
        return stream;
    }

};

#endif // GDSDBREADER_H
