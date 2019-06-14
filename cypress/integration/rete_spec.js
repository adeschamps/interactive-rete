/// <reference types="Cypress" />

context('Interactive Rete', () => {
  beforeEach(() => {
    cy.visit('/')
  })

  it('Title', () => {
    cy.title()
      .should('eq', 'Interactive Rete')
  })

  context('WMEs', () => {
    it('Enter a valid WME', () => {
      cy.contains('Enter a WME')
        .find('input')
        .type('(S1 ^superstate nil){enter}')
        .should('have.value', '')
    })
    
    it('Enter an invalid WME', () => {
      cy.contains('Enter a WME')
        .find('input')
        .type('not a wme{enter}')
        .should('have.value', 'not a wme')
    })
  })

  context('Productions', () => {
    it('Enter a valid production', () => {
      cy.contains('Add Production')
        .click()
      cy.contains('Enter a production')
        .find('textarea')
        .type('(<a> ^on <b>){enter}(<b> ^color blue)')
        .blur()
      cy.contains('problem =')
        .should('not.exist')
    })

    it('Enter an invalid production', () => {
      cy.contains('Add Production')
        .click()
      cy.contains('Enter a production')
        .find('textarea')
        .type('(<a> missing-caret <b>)')
        .blur()
      cy.contains('problem =')
    })
  })
})
