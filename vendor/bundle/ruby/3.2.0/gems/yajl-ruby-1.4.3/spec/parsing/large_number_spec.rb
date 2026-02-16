require 'spec_helper'

describe 'Parsing very long text' do
  shared_examples 'running script successfully' do |script|
    def dup_pipe(parent_half, child_half, new_io)
      parent_half.close
      new_io.reopen(child_half)
      child_half.close
    end

    def capture(cmd, stdin_data)
      child_in, child_out, child_err = IO::pipe, IO::pipe, IO::pipe

      child_pid = fork do
        dup_pipe(child_in[1], child_in[0], STDIN)
        dup_pipe(child_out[0], child_out[1], STDOUT)
        dup_pipe(child_err[0], child_err[1], STDERR)

        exec(cmd)
      end

      [
        child_in[0],
        child_out[1],
        child_err[1],
      ].each(&:close)

      child_in[1].write(stdin_data)
      child_in[1].close
      _, status = Process.waitpid2(child_pid)

      return child_out[0].read, child_err[0].read, status
    ensure
      [
        child_in[1],
        child_out[0],
        child_err[0],
      ].reject(&:closed?).each(&:close)
    end

    it 'runs successfully' do
      out, err, status = capture('ruby', script)
      expect([err, status.exitstatus]).to eq(['', 0])
    end
  end

  context 'when parseing big floats' do
    include_examples('running script successfully', <<-EOS)
require "yajl"
Yajl::Parser.parse('[0.' + '1' * 2**23 + ']')
    EOS
  end

  context 'when parseing long hash key with symbolize_keys option' do
    include_examples('running script successfully', <<-EOS)
require "yajl"
Yajl::Parser.parse('{"' + 'a' * 2**23 + '": 0}', :symbolize_keys => true)
    EOS
  end
end
