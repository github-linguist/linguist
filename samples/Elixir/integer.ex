defmodule Integer do
  @moduledoc """
  Functions for working with integers.
  """

  import Bitwise

  @doc """
  Determines if `integer` is odd.
  """
  defguard is_odd(integer) when is_integer(integer) and (integer &&& 1) == 1

  @doc """
  Determines if an `integer` is even.
  """
  defguard is_even(integer) when is_integer(integer) and (integer &&& 1) == 0

  @doc """
  Computes `base` raised to power of `exponent`.
  """
  @doc since: "1.12.0"
  @spec pow(integer, non_neg_integer) :: integer
  def pow(base, exponent) when is_integer(base) and is_integer(exponent) do
    if exponent < 0, do: :erlang.error(:badarith, [base, exponent])
    guarded_pow(base, exponent)
  end

  # https://en.wikipedia.org/wiki/Exponentiation_by_squaring
  defp guarded_pow(_, 0), do: 1
  defp guarded_pow(b, 1), do: b
  defp guarded_pow(b, e) when (e &&& 1) == 0, do: guarded_pow(b * b, e >>> 1)
  defp guarded_pow(b, e), do: b * guarded_pow(b * b, e >>> 1)

  @doc """
  Computes the modulo remainder of an integer division.
  """
  @doc since: "1.4.0"
  @spec mod(integer, neg_integer | pos_integer) :: integer
  def mod(dividend, divisor) do
    remainder = rem(dividend, divisor)

    if remainder * divisor < 0 do
      remainder + divisor
    else
      remainder
    end
  end

  @doc """
  Performs a floored integer division.
  """
  @doc since: "1.4.0"
  @spec floor_div(integer, neg_integer | pos_integer) :: integer
  def floor_div(dividend, divisor) do
    if dividend * divisor < 0 and rem(dividend, divisor) != 0 do
      div(dividend, divisor) - 1
    else
      div(dividend, divisor)
    end
  end

  @doc """
  Returns the ordered digits for the given `integer`.
  """
  @spec digits(integer, pos_integer) :: [integer, ...]
  def digits(integer, base \\ 10)
      when is_integer(integer) and is_integer(base) and base >= 2 do
    do_digits(integer, base, [])
  end

  defp do_digits(integer, base, acc) when abs(integer) < base, do: [integer | acc]

  defp do_digits(integer, base, acc),
    do: do_digits(div(integer, base), base, [rem(integer, base) | acc])

  @doc """
  Returns the integer represented by the ordered `digits`.
  """
  @spec undigits([integer], pos_integer) :: integer
  def undigits(digits, base \\ 10) when is_list(digits) and is_integer(base) and base >= 2 do
    do_undigits(digits, base, 0)
  end

  defp do_undigits([], _base, acc), do: acc

  defp do_undigits([digit | _], base, _) when is_integer(digit) and digit >= base,
    do: raise(ArgumentError, "invalid digit #{digit} in base #{base}")

  defp do_undigits([digit | tail], base, acc) when is_integer(digit),
    do: do_undigits(tail, base, acc * base + digit)

  @doc """
  Parses a text representation of an integer.
  """
  @spec parse(binary, 2..36) :: {integer, binary} | :error
  def parse(binary, base \\ 10)

  def parse(_binary, base) when base not in 2..36 do
    raise ArgumentError, "invalid base #{inspect(base)}"
  end

  def parse(binary, base) when is_binary(binary) do
    case count_digits(binary, base) do
      0 ->
        :error

      count ->
        {digits, rem} = :erlang.split_binary(binary, count)
        {:erlang.binary_to_integer(digits, base), rem}
    end
  end

  defp count_digits(<<sign, rest::bits>>, base) when sign in '+-' do
    case count_digits_nosign(rest, base, 1) do
      1 -> 0
      count -> count
    end
  end

  defp count_digits(<<rest::bits>>, base) do
    count_digits_nosign(rest, base, 0)
  end

  digits = [{?0..?9, -?0}, {?A..?Z, 10 - ?A}, {?a..?z, 10 - ?a}]

  for {chars, diff} <- digits,
      char <- chars do
    digit = char + diff

    defp count_digits_nosign(<<unquote(char), rest::bits>>, base, count)
         when base > unquote(digit) do
      count_digits_nosign(rest, base, count + 1)
    end
  end

  defp count_digits_nosign(<<_::bits>>, _, count), do: count

  # TODO: Remove Integer.to_string/1 once the minimum supported version is
  #       Erlang/OTP 22, since it is covered by the now BIF Integer.to_string/2.
  #       Please reapply commit 2622fd6b0aa419a983a899a1fbdb5deefba3d85d.
  @doc """
  Returns a binary which corresponds to the text representation
  of `integer`.
  """
  @spec to_string(integer) :: String.t()
  def to_string(integer) do
    :erlang.integer_to_binary(integer)
  end

  @doc """
  Returns a binary which corresponds to the text representation
  of `integer` in the given `base`.
  """
  @spec to_string(integer, 2..36) :: String.t()
  def to_string(integer, base) do
    :erlang.integer_to_binary(integer, base)
  end

  # TODO: Remove Integer.to_charlist/1 once the minimum supported version is
  #       Erlang/OTP 22, since it is covered by the now BIF Integer.to_charlist/2.
  #       Please reapply commit 2622fd6b0aa419a983a899a1fbdb5deefba3d85d.
  @doc """
  Returns a charlist which corresponds to the text representation of the given `integer`.
  """
  @spec to_charlist(integer) :: charlist
  def to_charlist(integer) do
    :erlang.integer_to_list(integer)
  end

  @doc """
  Returns a charlist which corresponds to the text representation of `integer` in the given `base`.
  """
  @spec to_charlist(integer, 2..36) :: charlist
  def to_charlist(integer, base) do
    :erlang.integer_to_list(integer, base)
  end

  @doc """
  Returns the greatest common divisor of the two given integers.
  """
  @doc since: "1.5.0"
  @spec gcd(integer, integer) :: non_neg_integer
  def gcd(integer1, integer2) when is_integer(integer1) and is_integer(integer2) do
    gcd_positive(abs(integer1), abs(integer2))
  end

  defp gcd_positive(0, integer2), do: integer2
  defp gcd_positive(integer1, 0), do: integer1
  defp gcd_positive(integer1, integer2), do: gcd_positive(integer2, rem(integer1, integer2))

  @doc false
  @deprecated "Use Integer.to_charlist/1 instead"
  def to_char_list(integer), do: Integer.to_charlist(integer)

  @doc false
  @deprecated "Use Integer.to_charlist/2 instead"
  def to_char_list(integer, base), do: Integer.to_charlist(integer, base)
end
