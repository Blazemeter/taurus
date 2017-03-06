```yaml
execution:
- concurrency: 250
  throughput: 500
  ramp-up: 3ms
  hold-for: 1h
  steps: 5
  scenario: blazemeter-recording

scenarios:
  blazemeter-recording:
    timeout: 5s
    retrieve-resources: false
    store-cache: false
    store-cookie: false
    default-address: http://blazedemo.com
    headers:
      User-Agent: 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36'
      Accept-Language: 'ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4'
      Accept-Encoding: 'gzip, deflate, sdch'
      Accept: 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp'
    requests:
    - /index.php
    - url: '/reserve.php'
      method: POST
      headers:
        Content-Type: application/x-www-form-urlencoded
      body:
        fromPort: Boston
        toPort: London
    - url: '/purchase.php'
      method: POST
      headers:
        Content-Type: application/x-www-form-urlencoded
      body:
        airline: United Airlines
        flight: '234'
        fromPort: Boston
        price: '432.98'
        toPort: London
    - url: '/confirmation.php'
      method: POST
      label: '/confirmation.php'
      headers:
        Content-Type: application/x-www-form-urlencoded
      body:
        address: test
        cardType: visa
        city: test
        creditCardNumber: test
        inputName: test
        nameOnCard: ettest
        rememberMe: 'on'
        state: test
        zipCode: test
    - /index.php
    - /vacation.html
```
