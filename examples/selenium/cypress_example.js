context('Example test', () => {
  it('visit blazedemo.com', () => {
  	cy.visit('http://blazedemo.com')
  	cy.wait(2000)
    cy.get('.btn-primary').click()
  	cy.wait(2000)
  })
})
