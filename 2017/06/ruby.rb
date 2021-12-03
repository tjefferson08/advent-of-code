require 'bundler/inline'
require 'minitest/autorun'

gemfile do
  source 'https://rubygems.org'
  gem 'minitest'
end

def count_redistributions(raw_input)
  block_arr = raw_input.split(/[\s+]/)
                .select { |str| !str.empty? }
                .map(&:strip)
                .map(&:to_i)
  puts "init #{block_arr}"
  count = 0
  distributions = {}
  current_distribution = block_arr
  prev_dist = nil
  while distributions["#{current_distribution}"].nil?
    distributions["#{current_distribution}"] = count
    # puts "setting #{current_distribution} to #{count}"
    current_distribution = redistribute_blocks(block_arr)
    count += 1
  end
  puts "dupe: #{current_distribution}"
  count - distributions["#{current_distribution}"]
end

def redistribute_blocks(block_arr)
  max_idx = max_index(block_arr)
  max_val = block_arr[max_idx]
  block_arr[max_idx] = 0
  puts "max val, idx: #{max_val}, #{max_idx}"
  chunks_to_distribute = max_val
  current_index = max_idx
  while chunks_to_distribute > 0
    current_index = (current_index + 1) % block_arr.length
    block_arr[current_index] += 1
    chunks_to_distribute -= 1
  end
  block_arr
end

def max_index(block_arr)
  max = block_arr.index(block_arr.max)
end

puts "soln: #{count_redistributions(File.read('input.txt'))}"

class TestMeme < Minitest::Test
  def test_things
    assert_equal([2, 4, 1, 2], redistribute_blocks([0, 2, 7, 0]))
    assert_equal([3, 1, 2, 3], redistribute_blocks([2, 4, 1, 2]))
    assert_equal([0, 2, 3, 4], redistribute_blocks([3, 1, 2, 3]))
    assert_equal([1, 3, 4, 1], redistribute_blocks([0, 2, 3, 4]))
    assert_equal([2, 4, 1, 2], redistribute_blocks([1, 3, 4, 1]))
    # assert_equal(5, count_redistributions("0\t2\t7\t0\n"))

  end

  def test_things_phase_2
    assert_equal(4, count_redistributions("0\t2\t7\t0\n"))
  end
end
