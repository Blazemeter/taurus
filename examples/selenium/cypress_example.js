context('Example test', () => {
  it('visit blazedemo.com', () => {
  	cy.visit('http://blazedemo.com')
    cy.get('.btn-primary').click()
  })
})
