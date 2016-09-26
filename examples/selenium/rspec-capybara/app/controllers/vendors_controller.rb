class VendorsController < ApplicationController
  def index
    @vendors = Vendor.all
  end
end
