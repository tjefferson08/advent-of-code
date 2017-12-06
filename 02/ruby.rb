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

text=File.open('input.txt').read

puts "soln: #{checksum(text)}"

class TestMeme < Minitest::Test
  def test_things
    assert_equal 18, checksum("5 1 9 5\n7 5 3\n2 4 6 8\n")
  end
end
