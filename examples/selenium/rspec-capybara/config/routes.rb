Rails.application.routes.draw do
  get '/vendors', to: 'vendors#index', as: 'vendors'
end
