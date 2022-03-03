defmodule Function do
  @moduledoc """
  A set of functions for working with functions.
  """

  @type information ::
          :arity
          | :env
          | :index
          | :module
          | :name
          | :new_index
          | :new_uniq
          | :pid
          | :type
          | :uniq

  @doc """
  Captures the given function.
  """
  @doc since: "1.7.0"
  @spec capture(module, atom, arity) :: fun
  def capture(module, function_name, arity) do
    :erlang.make_fun(module, function_name, arity)
  end

  @doc """
  Returns a keyword list with information about a function.
  """
  @doc since: "1.7.0"
  @spec info(fun) :: [{information, term}]
  def info(fun), do: :erlang.fun_info(fun)

  @doc """
  Returns a specific information about the function.
  """
  @doc since: "1.7.0"
  @spec info(fun, item) :: {item, term} when item: information
  def info(fun, item), do: :erlang.fun_info(fun, item)

  @doc """
  Returns its input `value`. This function can be passed as an anonymous function
  to transformation functions.
  """
  @doc since: "1.10.0"
  @spec identity(value) :: value when value: var
  def identity(value), do: value
end
