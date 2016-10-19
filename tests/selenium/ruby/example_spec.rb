RSpec.describe "my group" do

  specify "passing test" do
    expect(42).to eq(42)
  end

  specify "failing test" do
    expect(2 + 2 * 2).to eq(8)
  end

  specify "breaking test" do
    0 / 0
  end
end

