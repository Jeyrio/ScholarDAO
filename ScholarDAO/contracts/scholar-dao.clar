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

;; New map for student profiles
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

;; New map for milestone-based proposals
(define-map milestone-proposals
  { proposal-id: uint }
  {
    milestone-count: uint,
    milestone-amounts: (list 5 uint),
    milestones-completed: uint,
    current-milestone: uint
  }
)

;; New map for voting delegation
(define-map voting-delegates
  { delegator: principal }
  { delegate: principal }
)

;; Map for proposal categories
(define-map proposal-categories
  { proposal-id: uint }
  { category: (string-ascii 50) }
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
        created-at: stacks-block-height
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

;; NEW FUNCTION 1: Register student profile
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
        registration-date: stacks-block-height
      })
    (ok true)
  )
)

;; NEW FUNCTION 2: Emergency withdraw (owner only)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= (var-get total-fund) amount) err-insufficient-funds)
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (var-set total-fund (- (var-get total-fund) amount))
    (ok true)
  )
)

;; NEW FUNCTION 3: Batch vote on multiple proposals
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

;; NEW FUNCTION 4: Set proposal category/tag
(define-public (set-proposal-category (proposal-id uint) (category (string-ascii 50)))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get student proposal)) err-owner-only)
    (map-set proposal-categories { proposal-id: proposal-id } { category: category })
    (ok true)
  )
)

;; Enhanced read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-donor-contribution (donor principal))
  (default-to u0 (get amount (map-get? donor-contributions { donor: donor })))
)

(define-read-only (get-total-fund)
  (var-get total-fund)
)

;; NEW READ-ONLY FUNCTION: Get student profile
(define-read-only (get-student-profile (student principal))
  (map-get? student-profiles { student: student })
)

;; NEW READ-ONLY FUNCTION: Get proposal category
(define-read-only (get-proposal-category (proposal-id uint))
  (map-get? proposal-categories { proposal-id: proposal-id })
)

;; NEW READ-ONLY FUNCTION: Check if proposal is still active (within voting period)
(define-read-only (is-proposal-active (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (< (- stacks-block-height (get created-at proposal)) (var-get proposal-duration))
    false
  )
)

;; NEW READ-ONLY FUNCTION: Get proposal statistics
(define-read-only (get-proposal-stats (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (some {
      total-votes: (+ (get votes-for proposal) (get votes-against proposal)),
      vote-ratio: (if (> (+ (get votes-for proposal) (get votes-against proposal)) u0)
                    (/ (* (get votes-for proposal) u100) (+ (get votes-for proposal) (get votes-against proposal)))
                    u0),
      is-winning: (> (get votes-for proposal) (get votes-against proposal)),
      blocks-remaining: (if (>= (- stacks-block-height (get created-at proposal)) (var-get proposal-duration))
                          u0
                          (- (var-get proposal-duration) (- stacks-block-height (get created-at proposal))))
    })
    none
  )
)

;; NEW FUNCTION 5: Withdraw partial contribution (donor can reclaim unused funds)
(define-public (withdraw-contribution (amount uint))
  (let ((donor-contrib (get-donor-contribution tx-sender)))
    (asserts! (>= donor-contrib amount) err-insufficient-funds)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= (var-get total-fund) amount) err-insufficient-funds)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (var-set total-fund (- (var-get total-fund) amount))
    (map-set donor-contributions 
      { donor: tx-sender } 
      { amount: (- donor-contrib amount) })
    (ok true)
  )
)

;; NEW FUNCTION 6: Update proposal voting duration (owner only)
(define-public (update-proposal-duration (new-duration uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-duration u0) err-invalid-amount)
    (asserts! (<= new-duration u1440) err-invalid-amount) ;; Max 10 days (assuming 10 min blocks)
    (var-set proposal-duration new-duration)
    (ok new-duration)
  )
)

;; NEW READ-ONLY FUNCTION: Get voting history for a specific voter
(define-read-only (get-voter-history (voter principal) (proposal-id uint))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; NEW READ-ONLY FUNCTION: Get current proposal duration setting
(define-read-only (get-proposal-duration)
  (var-get proposal-duration)
)

;; NEW FUNCTION 7: Delegate voting power to another address
(define-public (delegate-voting-power (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) err-invalid-amount)
    (asserts! (> (get-donor-contribution tx-sender) u0) err-insufficient-funds)
    (map-set voting-delegates { delegator: tx-sender } { delegate: delegate })
    (ok true)
  )
)

;; NEW FUNCTION 8: Create milestone-based proposal with multiple payments
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
        created-at: stacks-block-height
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

;; NEW READ-ONLY FUNCTION: Get voting delegate for a user
(define-read-only (get-voting-delegate (delegator principal))
  (map-get? voting-delegates { delegator: delegator })
)

;; Get milestone proposal details
(define-read-only (get-milestone-proposal (proposal-id uint))
  (map-get? milestone-proposals { proposal-id: proposal-id })
)

;; Calculate total donations by all donors
(define-read-only (get-total-donations)
  (var-get total-fund)
)

;; Check if user can vote (either directly or as delegate)
(define-read-only (can-vote (voter principal) (proposal-id uint))
  (and 
    (is-some (map-get? proposals { proposal-id: proposal-id }))
    (is-none (map-get? votes { proposal-id: proposal-id, voter: voter }))
    (or 
      (> (get-donor-contribution voter) u0)
      (is-some (map-get? voting-delegates { delegator: voter }))
    )
  )
)