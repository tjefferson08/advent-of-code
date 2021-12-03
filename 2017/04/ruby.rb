
require 'bundler/inline'
require 'minitest/autorun'

gemfile do
  source 'https://rubygems.org'
  gem 'minitest'
end

def valid_passphrase(passphrase)
  passphrase_words = passphrase.split(" ").map(&:strip)
  passphrase_words.uniq.size == passphrase_words.size
end

def valid_passphrase_phase_2(passphrase)
  passphrase_words = passphrase.split(" ").map(&:strip)
  for i in (0..passphrase_words.length - 1)
    first_word = passphrase_words[i]
    for j in (i + 1..passphrase_words.length - 1)
      second_word = passphrase_words[j]
      return false if anagram_match(first_word, second_word)
    end
  end
  return true
end

def anagram_match(word_one, word_two)
  map_one = word_as_letter_map(word_one)
  map_two = word_as_letter_map(word_two)
  puts "match! #{word_one} and #{word_two}" if map_one == map_two
  map_one == map_two
end

def word_as_letter_map(word)
  word.chars.reduce({}) do |acc, letter|
    acc[letter] = acc[letter] ? acc[letter] + 1 : 1
    acc
  end
end

def sum_of_valid_passphrases
  count = 0
  File.foreach('input.txt') do |line|
    count += 1 if valid_passphrase_phase_2(line)
  end
  count
end

puts "soln: #{sum_of_valid_passphrases}"

class TestMeme < Minitest::Test
  def test_anagram_match
    assert_equal true, anagram_match('aa', 'aa')
    assert_equal false, anagram_match('aba', 'bab')
    assert_equal true, anagram_match('hellothere', 'ehlltoeher')
  end

  def test_things
    assert_equal true, valid_passphrase('aa bb cc dd ee')
    assert_equal false, valid_passphrase('aa bb cc dd aa')
    assert_equal true, valid_passphrase('aa bb cc dd aaa')
  end

  def test_things_phase_2
    assert_equal(true, valid_passphrase_phase_2('abcde fghij.'))
    assert_equal(false, valid_passphrase_phase_2('abcde xyz ecdab'))
    assert_equal(true, valid_passphrase_phase_2('a ab abc abd abf abj'))
    assert_equal(true, valid_passphrase_phase_2('iiii oiii ooii oooi oooo.'))
    assert_equal(false, valid_passphrase_phase_2('oiii ioii iioi iiio'))
  end
end
