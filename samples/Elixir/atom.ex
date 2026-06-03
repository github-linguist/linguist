defmodule Atom do
  @moduledoc """
  Atoms are constants whose values are their own name.
  """

  @doc """
  Converts an atom to a string.
  """
  @spec to_string(atom) :: String.t()
  def to_string(atom) do
    :erlang.atom_to_binary(atom, :utf8)
  end

  @doc """
  Converts an atom to a charlist.
  """
  @spec to_charlist(atom) :: charlist
  def to_charlist(atom) do
    :erlang.atom_to_list(atom)
  end

  @doc false
  @deprecated "Use Atom.to_charlist/1 instead"
  @spec to_char_list(atom) :: charlist
  def to_char_list(atom), do: Atom.to_charlist(atom)
end
