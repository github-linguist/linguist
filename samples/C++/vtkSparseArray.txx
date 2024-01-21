/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkSparseArray.txx

-------------------------------------------------------------------------
  Copyright 2008 Sandia Corporation.
  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
  the U.S. Government retains certain rights in this software.
-------------------------------------------------------------------------

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#ifndef vtkSparseArray_txx
#define vtkSparseArray_txx

#include <algorithm>
#include <limits>

VTK_ABI_NAMESPACE_BEGIN
template <typename T>
vtkSparseArray<T>* vtkSparseArray<T>::New()
{
  // Don't use object factory macros on templates, it'll confuse the object
  // factory.
  vtkSparseArray<T>* ret = new vtkSparseArray<T>;
  ret->InitializeObjectBase();
  return ret;
}

template <typename T>
void vtkSparseArray<T>::PrintSelf(ostream& os, vtkIndent indent)
{
  vtkSparseArray<T>::Superclass::PrintSelf(os, indent);
}

template <typename T>
bool vtkSparseArray<T>::IsDense()
{
  return false;
}

template <typename T>
const vtkArrayExtents& vtkSparseArray<T>::GetExtents()
{
  return this->Extents;
}

template <typename T>
typename vtkSparseArray<T>::SizeT vtkSparseArray<T>::GetNonNullSize()
{
  return this->Values.size();
}

template <typename T>
void vtkSparseArray<T>::GetCoordinatesN(SizeT n, vtkArrayCoordinates& coordinates)
{
  coordinates.SetDimensions(this->GetDimensions());
  for (DimensionT i = 0; i != this->GetDimensions(); ++i)
    coordinates[i] = this->Coordinates[i][n];
}

template <typename T>
vtkArray* vtkSparseArray<T>::DeepCopy()
{
  ThisT* const copy = ThisT::New();

  copy->SetName(this->GetName());
  copy->Extents = this->Extents;
  copy->DimensionLabels = this->DimensionLabels;
  copy->Coordinates = this->Coordinates;
  copy->Values = this->Values;
  copy->NullValue = this->NullValue;

  return copy;
}

template <typename T>
const T& vtkSparseArray<T>::GetValue(CoordinateT i)
{
  if (1 != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return this->NullValue;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    if (i != this->Coordinates[0][row])
      continue;

    return this->Values[row];
  }

  return this->NullValue;
}

template <typename T>
const T& vtkSparseArray<T>::GetValue(CoordinateT i, CoordinateT j)
{
  if (2 != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return this->NullValue;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    if (i != this->Coordinates[0][row])
      continue;

    if (j != this->Coordinates[1][row])
      continue;

    return this->Values[row];
  }

  return this->NullValue;
}

template <typename T>
const T& vtkSparseArray<T>::GetValue(CoordinateT i, CoordinateT j, CoordinateT k)
{
  if (3 != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return this->NullValue;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    if (i != this->Coordinates[0][row])
      continue;

    if (j != this->Coordinates[1][row])
      continue;

    if (k != this->Coordinates[2][row])
      continue;

    return this->Values[row];
  }

  return this->NullValue;
}

template <typename T>
const T& vtkSparseArray<T>::GetValue(const vtkArrayCoordinates& coordinates)
{
  if (coordinates.GetDimensions() != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return this->NullValue;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    for (DimensionT column = 0; column != this->GetDimensions(); ++column)
    {
      if (coordinates[column] != this->Coordinates[column][row])
        break;

      if (column + 1 == this->GetDimensions())
        return this->Values[row];
    }
  }

  return this->NullValue;
}

template <typename T>
const T& vtkSparseArray<T>::GetValueN(SizeT n)
{
  return this->Values[n];
}

template <typename T>
void vtkSparseArray<T>::SetValue(CoordinateT i, const T& value)
{
  if (1 != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    if (i != this->Coordinates[0][row])
      continue;

    this->Values[row] = value;
    return;
  }

  // Element doesn't already exist, so add it to the end of the list ...
  this->AddValue(i, value);
}

template <typename T>
void vtkSparseArray<T>::SetValue(CoordinateT i, CoordinateT j, const T& value)
{
  if (2 != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    if (i != this->Coordinates[0][row])
      continue;

    if (j != this->Coordinates[1][row])
      continue;

    this->Values[row] = value;
    return;
  }

  // Element doesn't already exist, so add it to the end of the list ...
  this->AddValue(i, j, value);
}

template <typename T>
void vtkSparseArray<T>::SetValue(CoordinateT i, CoordinateT j, CoordinateT k, const T& value)
{
  if (3 != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    if (i != this->Coordinates[0][row])
      continue;

    if (j != this->Coordinates[1][row])
      continue;

    if (k != this->Coordinates[2][row])
      continue;

    this->Values[row] = value;
    return;
  }

  // Element doesn't already exist, so add it to the end of the list ...
  this->AddValue(i, j, k, value);
}

template <typename T>
void vtkSparseArray<T>::SetValue(const vtkArrayCoordinates& coordinates, const T& value)
{
  if (coordinates.GetDimensions() != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return;
  }

  // Do a naive linear-search for the time-being ...
  for (vtkIdType row = 0; row != static_cast<vtkIdType>(this->Values.size()); ++row)
  {
    for (DimensionT column = 0; column != this->GetDimensions(); ++column)
    {
      if (coordinates[column] != this->Coordinates[column][row])
        break;

      if (column + 1 == this->GetDimensions())
      {
        this->Values[row] = value;
        return;
      }
    }
  }

  // Element doesn't already exist, so add it to the end of the list ...
  this->AddValue(coordinates, value);
}

template <typename T>
void vtkSparseArray<T>::SetValueN(SizeT n, const T& value)
{
  this->Values[n] = value;
}

template <typename T>
void vtkSparseArray<T>::SetNullValue(const T& value)
{
  this->NullValue = value;
}

template <typename T>
const T& vtkSparseArray<T>::GetNullValue()
{
  return this->NullValue;
}

template <typename T>
void vtkSparseArray<T>::Clear()
{
  for (DimensionT column = 0; column != this->GetDimensions(); ++column)
    this->Coordinates[column].clear();

  this->Values.clear();
}

/// Predicate object for use with std::sort().  Given a vtkArraySort object that defines which array
/// dimensions will be sorted in what order, SortCoordinates is used to establish a sorted order for
/// the values stored in vtkSparseArray. Note that SortCoordinates never actually modifies its
/// inputs.
struct SortCoordinates
{
  SortCoordinates(const vtkArraySort& sort, const std::vector<std::vector<vtkIdType>>& coordinates)
    : Sort(&sort)
    , Coordinates(&coordinates)
  {
  }

  bool operator()(const vtkIdType lhs, const vtkIdType rhs) const
  {
    const vtkArraySort& sort = *this->Sort;
    const std::vector<std::vector<vtkIdType>>& coordinates = *this->Coordinates;

    for (vtkIdType i = 0; i != sort.GetDimensions(); ++i)
    {
      if (coordinates[sort[i]][lhs] == coordinates[sort[i]][rhs])
        continue;

      return coordinates[sort[i]][lhs] < coordinates[sort[i]][rhs];
    }

    return false;
  }

  const vtkArraySort* Sort;
  const std::vector<std::vector<vtkIdType>>* Coordinates;
};

template <typename T>
void vtkSparseArray<T>::Sort(const vtkArraySort& sort)
{
  if (sort.GetDimensions() < 1)
  {
    vtkErrorMacro(<< "Sort must order at least one dimension.");
    return;
  }

  for (DimensionT i = 0; i != sort.GetDimensions(); ++i)
  {
    if (sort[i] < 0 || sort[i] >= this->GetDimensions())
    {
      vtkErrorMacro(<< "Sort dimension out-of-bounds.");
      return;
    }
  }

  const SizeT count = this->GetNonNullSize();
  std::vector<DimensionT> sort_order(count);
  for (SizeT i = 0; i != count; ++i)
    sort_order[i] = static_cast<DimensionT>(i);
  std::sort(sort_order.begin(), sort_order.end(), SortCoordinates(sort, this->Coordinates));

  std::vector<DimensionT> temp_coordinates(count);
  for (DimensionT j = 0; j != this->GetDimensions(); ++j)
  {
    for (SizeT i = 0; i != count; ++i)
      temp_coordinates[i] = this->Coordinates[j][sort_order[i]];
    std::swap(temp_coordinates, this->Coordinates[j]);
  }

  std::vector<T> temp_values(count);
  for (SizeT i = 0; i != count; ++i)
    temp_values[i] = this->Values[sort_order[i]];
  std::swap(temp_values, this->Values);
}

template <typename T>
std::vector<typename vtkSparseArray<T>::CoordinateT> vtkSparseArray<T>::GetUniqueCoordinates(
  DimensionT dimension)
{
  if (dimension < 0 || dimension >= this->GetDimensions())
  {
    vtkErrorMacro(<< "Dimension out-of-bounds.");
    return std::vector<CoordinateT>();
  }

  std::vector<CoordinateT> results(this->Coordinates[dimension]);
  std::sort(results.begin(), results.end());
  results.erase(std::unique(results.begin(), results.end()), results.end());
  return results;
}

template <typename T>
const typename vtkSparseArray<T>::CoordinateT* vtkSparseArray<T>::GetCoordinateStorage(
  DimensionT dimension) const
{
  if (dimension < 0 || dimension >= this->GetDimensions())
  {
    vtkErrorMacro(<< "Dimension out-of-bounds.");
    return nullptr;
  }

  return &this->Coordinates[dimension][0];
}

template <typename T>
typename vtkSparseArray<T>::CoordinateT* vtkSparseArray<T>::GetCoordinateStorage(
  DimensionT dimension)
{
  if (dimension < 0 || dimension >= this->GetDimensions())
  {
    vtkErrorMacro(<< "Dimension out-of-bounds.");
    return nullptr;
  }

  return &this->Coordinates[dimension][0];
}

template <typename T>
const T* vtkSparseArray<T>::GetValueStorage() const
{
  return &(this->Values[0]);
}

template <typename T>
T* vtkSparseArray<T>::GetValueStorage()
{
  return &this->Values[0];
}

template <typename T>
void vtkSparseArray<T>::ReserveStorage(SizeT value_count)
{
  for (DimensionT dimension = 0; dimension != this->GetDimensions(); ++dimension)
    this->Coordinates[dimension].resize(value_count);

  this->Values.resize(value_count);
}

template <typename T>
void vtkSparseArray<T>::SetExtentsFromContents()
{
  vtkArrayExtents new_extents;

  const vtkIdType row_begin = 0;
  const vtkIdType row_end = row_begin + static_cast<vtkIdType>(this->Values.size());
  const DimensionT dimension_count = this->GetDimensions();
  for (DimensionT dimension = 0; dimension != dimension_count; ++dimension)
  {
    vtkIdType range_begin = std::numeric_limits<vtkIdType>::max();
    vtkIdType range_end = -std::numeric_limits<vtkIdType>::max();
    for (vtkIdType row = row_begin; row != row_end; ++row)
    {
      range_begin = std::min(range_begin, this->Coordinates[dimension][row]);
      range_end = std::max(range_end, this->Coordinates[dimension][row] + 1);
    }
    new_extents.Append(vtkArrayRange(range_begin, range_end));
  }

  this->Extents = new_extents;
}

template <typename T>
void vtkSparseArray<T>::SetExtents(const vtkArrayExtents& extents)
{
  if (extents.GetDimensions() != this->GetDimensions())
  {
    vtkErrorMacro(<< "Extent-array dimension mismatch.");
    return;
  }

  this->Extents = extents;
}

template <typename T>
void vtkSparseArray<T>::AddValue(CoordinateT i, const T& value)
{
  this->AddValue(vtkArrayCoordinates(i), value);
}

template <typename T>
void vtkSparseArray<T>::AddValue(CoordinateT i, CoordinateT j, const T& value)
{
  this->AddValue(vtkArrayCoordinates(i, j), value);
}

template <typename T>
void vtkSparseArray<T>::AddValue(CoordinateT i, CoordinateT j, CoordinateT k, const T& value)
{
  this->AddValue(vtkArrayCoordinates(i, j, k), value);
}

template <typename T>
void vtkSparseArray<T>::AddValue(const vtkArrayCoordinates& coordinates, const T& value)
{
  if (coordinates.GetDimensions() != this->GetDimensions())
  {
    vtkErrorMacro(<< "Index-array dimension mismatch.");
    return;
  }

  this->Values.push_back(value);

  for (DimensionT i = 0; i != coordinates.GetDimensions(); ++i)
    this->Coordinates[i].push_back(coordinates[i]);
}

template <typename T>
bool vtkSparseArray<T>::Validate()
{
  vtkIdType duplicate_count = 0;
  vtkIdType out_of_bound_count = 0;

  const vtkIdType dimensions = this->GetDimensions();
  const vtkIdType count = this->GetNonNullSize();

  // Create an arbitrary sorted order for our coordinates ...
  vtkArraySort sort;
  sort.SetDimensions(dimensions);
  for (vtkIdType i = 0; i != dimensions; ++i)
    sort[i] = i;

  std::vector<vtkIdType> sort_order(count);
  for (vtkIdType i = 0; i != count; ++i)
    sort_order[i] = i;

  std::sort(sort_order.begin(), sort_order.end(), SortCoordinates(sort, this->Coordinates));

  // Now, look for duplicates ...
  for (vtkIdType i = 0; i + 1 < count; ++i)
  {
    vtkIdType j;
    for (j = 0; j != dimensions; ++j)
    {
      if (this->Coordinates[j][sort_order[i]] != this->Coordinates[j][sort_order[i + 1]])
        break;
    }
    if (j == dimensions)
    {
      duplicate_count += 1;
    }
  }

  // Look for out-of-bound coordinates ...
  for (vtkIdType i = 0; i != count; ++i)
  {
    for (vtkIdType j = 0; j != dimensions; ++j)
    {
      if (this->Coordinates[j][i] < this->Extents[j].GetBegin() ||
        this->Coordinates[j][i] >= this->Extents[j].GetEnd())
      {
        ++out_of_bound_count;
        break;
      }
    }
  }

  if (duplicate_count)
  {
    vtkErrorMacro(<< "Array contains " << duplicate_count << " duplicate coordinates.");
  }

  if (out_of_bound_count)
  {
    vtkErrorMacro(<< "Array contains " << out_of_bound_count << " out-of-bound coordinates.");
  }

  return (0 == duplicate_count) && (0 == out_of_bound_count);
}

template <typename T>
vtkSparseArray<T>::vtkSparseArray()
  : NullValue(T())
{
}

template <typename T>
vtkSparseArray<T>::~vtkSparseArray() = default;

template <typename T>
void vtkSparseArray<T>::InternalResize(const vtkArrayExtents& extents)
{
  this->Extents = extents;
  this->DimensionLabels.resize(extents.GetDimensions(), {});
  this->Coordinates.resize(extents.GetDimensions());
  this->Values.resize(0);
}

template <typename T>
void vtkSparseArray<T>::InternalSetDimensionLabel(DimensionT i, const vtkStdString& label)
{
  this->DimensionLabels[i] = label;
}

template <typename T>
vtkStdString vtkSparseArray<T>::InternalGetDimensionLabel(DimensionT i)
{
  return this->DimensionLabels[i];
}

VTK_ABI_NAMESPACE_END
#endif
