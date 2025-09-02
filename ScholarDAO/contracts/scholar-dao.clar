;; ScholarDAO - Decentralized scholarship fund
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-proposal-expired (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-already-exists (err u106))

(define-data-var next-proposal-id uint u0)
(define-data-var total-fund uint u0)
(define-data-var proposal-duration uint u144) ;; ~24 hours in blocks (assuming 10 min blocks)

(define-map proposals
  { proposal-id: uint }
  {
    student: principal,
    amount: uint,
    description: (string-ascii 500),
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    created-at: uint
  }
)

(define-map donor-contributions
  { donor: principal }
  { amount: uint }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool }
)

(define-map student-profiles
  { student: principal }
  {
    name: (string-ascii 100),
    institution: (string-ascii 200),
    field-of-study: (string-ascii 100),
    gpa: uint,
    registration-date: uint
  }
)

(define-map milestone-proposals
  { proposal-id: uint }
  {
    milestone-count: uint,
    milestone-amounts: (list 5 uint),
    milestones-completed: uint,
    current-milestone: uint
  }
)

(define-map voting-delegates
  { delegator: principal }
  { delegate: principal }
)