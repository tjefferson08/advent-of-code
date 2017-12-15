require 'bundler/inline'
require 'minitest/autorun'

gemfile do
  source 'https://rubygems.org'
  gem 'minitest'
end

def count_jumps(raw_input)
  offset_arr = raw_input.split("\n").map(&:to_i)
  current_index, jump_count = [0, 0]
  while current_index < offset_arr.length
    jump_length = offset_arr[current_index]
    index_after_jump = current_index + jump_length
    if jump_length >= 3
      offset_arr[current_index] -= 1
    else
      offset_arr[current_index] += 1
    end
    current_index = index_after_jump
    jump_count += 1
  end

  jump_count
end

puts "soln: #{count_jumps(File.read('input.txt'))}"

class TestMeme < Minitest::Test
  def test_things
    # assert_equal(5, count_jumps("0\n3\n0\n1\n-3\n"))
  end

  def test_things_phase_2
    assert_equal(10, count_jumps("0\n3\n0\n1\n-3\n"))
  end
end
