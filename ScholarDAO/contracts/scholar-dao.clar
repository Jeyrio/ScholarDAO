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

(define-public (contribute (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set total-fund (+ (var-get total-fund) amount))
    (map-set donor-contributions { donor: tx-sender } { amount: (+ amount (get-donor-contribution tx-sender)) })
    (ok true)
  )
)

(define-public (submit-proposal (amount uint) (description (string-ascii 500)))
  (let ((proposal-id (var-get next-proposal-id)))
    (map-set proposals
      { proposal-id: proposal-id }
      {
        student: tx-sender,
        amount: amount,
        description: description,
        votes-for: u0,
        votes-against: u0,
        executed: false,
        created-at: block-height
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (support bool))
  (let ((voter tx-sender))
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: voter })) err-already-voted)
    (asserts! (is-some (map-get? proposals { proposal-id: proposal-id })) err-not-found)
    (map-set votes { proposal-id: proposal-id, voter: voter } { vote: support })
    (if support
      (map-set proposals
        { proposal-id: proposal-id }
        (merge (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))
               { votes-for: (+ (get votes-for (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))) u1) }))
      (map-set proposals
        { proposal-id: proposal-id }
        (merge (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))
               { votes-against: (+ (get votes-against (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))) u1) })))
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found)))
    (asserts! (not (get executed proposal)) err-not-found)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) err-not-found)
    (asserts! (>= (var-get total-fund) (get amount proposal)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get student proposal))))
    (var-set total-fund (- (var-get total-fund) (get amount proposal)))
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true }))
    (ok true)
  )
)

(define-public (register-student-profile 
  (name (string-ascii 100)) 
  (institution (string-ascii 200)) 
  (field-of-study (string-ascii 100)) 
  (gpa uint))
  (begin
    (asserts! (is-none (map-get? student-profiles { student: tx-sender })) err-already-exists)
    (asserts! (<= gpa u400) err-invalid-amount) ;; GPA should be <= 4.00 (represented as 400)
    (map-set student-profiles
      { student: tx-sender }
      {
        name: name,
        institution: institution,
        field-of-study: field-of-study,
        gpa: gpa,
        registration-date: block-height
      })
    (ok true)
  )
)

(define-public (batch-vote (proposal-votes (list 10 { proposal-id: uint, support: bool })))
  (begin
    (fold batch-vote-helper proposal-votes (ok true))
  )
)

(define-private (batch-vote-helper (vote-data { proposal-id: uint, support: bool }) (prev-result (response bool uint)))
  (match prev-result
    success (vote (get proposal-id vote-data) (get support vote-data))
    error-val (err error-val)
  )
)

(define-public (delegate-voting-power (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) err-invalid-amount)
    (asserts! (> (get-donor-contribution tx-sender) u0) err-insufficient-funds)
    (map-set voting-delegates { delegator: tx-sender } { delegate: delegate })
    (ok true)
  )
)

(define-public (submit-milestone-proposal 
  (total-amount uint) 
  (description (string-ascii 500))
  (milestone-count uint)
  (milestone-amounts (list 5 uint)))
  (let ((proposal-id (var-get next-proposal-id)))
    (asserts! (> milestone-count u0) err-invalid-amount)
    (asserts! (<= milestone-count u5) err-invalid-amount)
    (asserts! (is-eq (len milestone-amounts) milestone-count) err-invalid-amount)
    (asserts! (is-eq total-amount (fold + milestone-amounts u0)) err-invalid-amount)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        student: tx-sender,
        amount: total-amount,
        description: description,
        votes-for: u0,
        votes-against: u0,
        executed: false,
        created-at: block-height
      }
    )
    
    (map-set milestone-proposals
      { proposal-id: proposal-id }
      {
        milestone-count: milestone-count,
        milestone-amounts: milestone-amounts,
        milestones-completed: u0,
        current-milestone: u0
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)
