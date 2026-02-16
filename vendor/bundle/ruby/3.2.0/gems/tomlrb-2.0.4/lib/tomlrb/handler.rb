# frozen-string-literal: true

module Tomlrb
  class Handler
    attr_reader :output, :symbolize_keys

    def initialize(**options)
      @output = {}
      @current = @output
      @stack = []
      @array_names = []
      @current_table = []
      @keys = Keys.new
      @symbolize_keys = options[:symbolize_keys]
    end

    def set_context(identifiers, is_array_of_tables: false)
      if identifiers.empty?
        raise ParseError, 'Array needs a name'
      end

      @current_table = identifiers.dup
      @keys.add_table_key identifiers, is_array_of_tables
      @current = @output

      deal_with_array_of_tables(identifiers, is_array_of_tables) do |identifierz|
        identifierz.each do |k|
          k = k.to_sym if @symbolize_keys
          if @current[k].is_a?(Array)
            @current = @current[k].last
          else
            @current[k] ||= {}
            @current = @current[k]
          end
        end
      end
    end

    def deal_with_array_of_tables(identifiers, is_array_of_tables)
      stringified_identifier = identifiers.join('.')

      if is_array_of_tables
        @array_names << stringified_identifier
        last_identifier = identifiers.pop
      elsif @array_names.include?(stringified_identifier)
        raise ParseError, 'Cannot define a normal table with the same name as an already established array'
      end

      yield(identifiers)

      if is_array_of_tables
        last_identifier = last_identifier.to_sym if @symbolize_keys
        @current[last_identifier] ||= []
        raise ParseError, "Cannot use key #{last_identifier} for both table and array at once" unless @current[last_identifier].respond_to?(:<<)
        @current[last_identifier] << {}
        @current = @current[last_identifier].last
      end
    end

    def assign(k)
      @keys.add_pair_key k, @current_table
      current = @current
      while (key = k.shift)
        key = key.to_sym if @symbolize_keys
        current = assign_key_path(current, key, k.empty?)
      end
    end

    def push(o)
      @stack << o
    end

    def push_inline(inline_arrays)
      merged_inline = {}
      keys = Keys.new

      inline_arrays.each do |inline_array|
        current = merged_inline
        value = inline_array.pop
        keys.add_table_key inline_array, value.is_a?(Array)

        inline_array.each_with_index do |inline_key, inline_index|
          inline_key = inline_key.to_sym if @symbolize_keys
          last_key = inline_index == inline_array.size - 1

          if last_key
            if current[inline_key].nil?
              keys.add_pair_key [inline_key], []

              current[inline_key] = value
            else
              raise Key::KeyConflict, "Inline key #{inline_key} is already used"
            end
          else
            current[inline_key] ||= {}
            current = current[inline_key]
          end
        end
      end

      push(merged_inline)
    end

    def start_(type)
      push([type])
    end

    def end_(type)
      array = []
      while (value = @stack.pop) != [type]
        raise ParseError, 'Unclosed table' if @stack.empty?

        array.unshift(value)
      end
      array
    end

    def validate_value(value)
      if value.nil?
        raise ParseError, 'Value must be present'
      end
    end

    private

    def assign_key_path(current, key, key_emptied)
      existed = current.key?(key)
      raise ParseError, "Cannot overwrite value with key #{key}" if existed && !current[key].is_a?(Hash)
      if key_emptied
        raise ParseError, "Cannot overwrite value with key #{key}" unless current.is_a?(Hash)

        value = @stack.pop
        raise ParseError, "Cannot overwrite value with key #{key}" if current[key].is_a?(Hash) && !value.is_a?(Hash)
        current[key] = value
        return current
      end
      current[key] ||= {}
      current[key]
    end
  end

  class Keys
    def initialize
      @keys = {}
    end

    def add_table_key(keys, is_array_of_tables = false)
      self << [keys, [], is_array_of_tables]
    end

    def add_pair_key(keys, context)
      self << [context, keys, false]
    end

    def <<(keys)
      table_keys, pair_keys, is_array_of_tables = keys
      current = @keys
      current = append_table_keys(current, table_keys, pair_keys.empty?, is_array_of_tables)
      append_pair_keys(current, pair_keys, table_keys.empty?, is_array_of_tables)
    end

    private

    def append_table_keys(current, table_keys, pair_keys_empty, is_array_of_tables)
      table_keys.each_with_index do |key, index|
        declared = (index == table_keys.length - 1) && pair_keys_empty
        if index.zero?
          current = find_or_create_first_table_key(current, key, declared, is_array_of_tables)
        else
          current <<= [key, :table, declared, is_array_of_tables]
        end
      end

      current.clear_children if is_array_of_tables
      current
    end

    def find_or_create_first_table_key(current, key, declared, is_array_of_tables)
      existed = current[key]
      if existed && existed.type == :pair
        raise Key::KeyConflict, "Key #{key} is already used as #{existed.type} key"
      end
      if existed && existed.declared? && declared && ! is_array_of_tables
        raise Key::KeyConflict, "Key #{key} is already used"
      end
      k = existed || Key.new(key, :table, declared)
      k.declared = k.declared? || declared
      current[key] = k
      k
    end

    def append_pair_keys(current, pair_keys, table_keys_empty, is_array_of_tables)
      pair_keys.each_with_index do |key, index|
        declared = index == pair_keys.length - 1
        if index.zero? && table_keys_empty
          current = find_or_create_first_pair_key(current, key, declared, table_keys_empty)
        else
          key = current << [key, :pair, declared, is_array_of_tables]
          current = key
        end
      end
    end

    def find_or_create_first_pair_key(current, key, declared, table_keys_empty)
      existed = current[key]
      if existed && (existed.type == :pair) && declared && table_keys_empty
        raise Key::KeyConflict, "Key #{key} is already used"
      end
      k = Key.new(key, :pair, declared)
      current[key] = k
      k
    end
  end

  class Key
    class KeyConflict < ParseError; end

    attr_reader :key, :type
    attr_writer :declared

    def initialize(key, type, declared = false)
      @key = key
      @type = type
      @declared = declared
      @children = {}
    end

    def declared?
      @declared
    end

    def <<(key_type_declared)
      key, type, declared, is_array_of_tables = key_type_declared
      existed = @children[key]
      validate_already_declared_as_different_key(type, declared, existed)
      validate_already_declared_as_non_array_table(type, is_array_of_tables, declared, existed)
      validate_path_already_created_as_different_type(type, declared, existed)
      validate_path_already_declared_as_different_type(type, declared, existed)
      validate_already_declared_as_same_key(declared, existed)
      @children[key] = existed || self.class.new(key, type, declared)
    end

    def clear_children
      @children.clear
    end

    private

    def validate_already_declared_as_different_key(type, _declared, existed)
      if existed && existed.declared? && existed.type != type
        raise KeyConflict, "Key #{existed.key} is already used as #{existed.type} key"
      end
    end

    def validate_already_declared_as_non_array_table(type, is_array_of_tables, declared, existed)
      if declared && type == :table && existed && existed.declared? && !is_array_of_tables
        raise KeyConflict, "Key #{existed.key} is already used"
      end
    end

    def validate_path_already_created_as_different_type(type, declared, existed)
      if declared && (type == :table) && existed && (existed.type == :pair) && !existed.declared?
        raise KeyConflict, "Key #{existed.key} is already used as #{existed.type} key"
      end
    end

    def validate_path_already_declared_as_different_type(type, declared, existed)
      if !declared && (type == :pair) && existed && (existed.type == :pair) && existed.declared?
        raise KeyConflict, "Key #{key} is already used as #{type} key"
      end
    end

    def validate_already_declared_as_same_key(declared, existed)
      if existed && !existed.declared? && declared
        raise KeyConflict, "Key #{existed.key} is already used as #{existed.type} key"
      end
    end
  end
end
