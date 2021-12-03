require 'bundler/inline'
require 'minitest/autorun'

gemfile do
  source 'https://rubygems.org'
  gem 'minitest'
end

def checksum(raw_input)
  raw_input.gsub!(/\r\n?/, "\n")
  raw_input.split("\n").map do |line|
    row_numbers = line.split.map(&:strip).map(&:to_i)
    row_numbers.max - row_numbers.min
  end.reduce(:+)
end

def checksum_phase_2(raw_input)
  raw_input.gsub!(/\r\n?/, "\n")
  raw_input.split("\n").map do |line|
    row_numbers = line.split.map(&:strip).map(&:to_i)
    find_evenly_divisible_pair_and_divide(row_numbers)
  end.reduce(:+)
end

def find_evenly_divisible_pair_and_divide(num_arr)
  for i in (0..num_arr.length - 1)
    for j in ((i + 1)..num_arr.length - 1)
      puts "comparing indices #{i}, #{j}"
      if num_arr[i] % num_arr[j] == 0
        puts "found the pair! #{num_arr[i]}, #{num_arr[j]}"
        return num_arr[i] / num_arr[j]
      elsif num_arr[j] % num_arr[i] == 0
        puts "found the pair! #{num_arr[i]}, #{num_arr[j]}"
        return num_arr[j] / num_arr[i]
      end
    end
  end
end

text=File.open('input.txt').read

puts "soln: #{checksum(text)}"
puts "soln phase 2: #{checksum_phase_2(text)}"

class TestMeme < Minitest::Test
  def test_things
    assert_equal 18, checksum("5 1 9 5\n7 5 3\n2 4 6 8\n")
  end

  def test_things_phase_2
    assert_equal(9, checksum_phase_2("5 9 2 8\n9 4 7 3\n3 8 6 5\n"))
  end
end
