$subject_number = 7
$divisor = 20201227

card_public_key = 8987316
door_public_key = 14681524

def find_loop_size(public_key)
  value = 1
  loop_size = 0
  until value == public_key
    value = (value * $subject_number) % $divisor
    loop_size = loop_size + 1
  end
  loop_size
end

def calculate_encryption_key(loop_size, public_key)
  loops = 0
  value = 1
  until loops == loop_size
    value = (value * public_key) % $divisor
    loops = loops + 1
  end
  value
end

card_loop_size = find_loop_size(card_public_key)
encryption_key = calculate_encryption_key(card_loop_size, door_public_key)

puts encryption_key
