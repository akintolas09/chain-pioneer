;; ChainPioneer - Dynamic NFT Card Game Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-not-found (err u102))
(define-constant err-already-minted (err u103))
(define-constant err-insufficient-balance (err u104))

;; Data Variables
(define-data-var last-token-id uint u0)
(define-data-var base-mint-price uint u1000000) ;; 1 STX in microSTX

;; Data Maps
;; NFT ownership tracking
(define-map token-owners uint principal)

;; Card attributes - stored as separate maps for flexibility
(define-map card-power uint uint)
(define-map card-speed uint uint)
(define-map card-defense uint uint)
(define-map card-rarity uint (string-ascii 20))
(define-map card-element uint (string-ascii 20))

;; Dynamic attribute multipliers (simulating oracle data)
(define-map attribute-multipliers (string-ascii 20) uint)

;; Token URI mapping
(define-map token-uris uint (string-ascii 256))

;; Player reputation scores
(define-map player-reputation principal uint)

;; Read-only functions

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-read-only (get-owner (token-id uint))
  (ok (map-get? token-owners token-id))
)

(define-read-only (get-card-attributes (token-id uint))
  (ok {
    power: (default-to u0 (map-get? card-power token-id)),
    speed: (default-to u0 (map-get? card-speed token-id)),
    defense: (default-to u0 (map-get? card-defense token-id)),
    rarity: (default-to "common" (map-get? card-rarity token-id)),
    element: (default-to "neutral" (map-get? card-element token-id))
  })
)

(define-read-only (get-token-uri (token-id uint))
  (ok (map-get? token-uris token-id))
)

(define-read-only (get-player-reputation (player principal))
  (ok (default-to u0 (map-get? player-reputation player)))
)

(define-read-only (get-attribute-multiplier (attribute (string-ascii 20)))
  (ok (default-to u100 (map-get? attribute-multipliers attribute)))
)

(define-read-only (calculate-effective-power (token-id uint))
  (let (
    (base-power (default-to u0 (map-get? card-power token-id)))
    (multiplier (default-to u100 (map-get? attribute-multipliers "power")))
  )
    (ok (/ (* base-power multiplier) u100))
  )
)

;; Public functions

(define-public (mint-card 
  (power uint)
  (speed uint)
  (defense uint)
  (rarity (string-ascii 20))
  (element (string-ascii 20))
  (token-uri (string-ascii 256))
)
  (let (
    (token-id (+ (var-get last-token-id) u1))
    (mint-price (var-get base-mint-price))
  )
    ;; Transfer STX payment to contract
    (try! (stx-transfer? mint-price tx-sender contract-owner))
    
    ;; Set ownership
    (map-set token-owners token-id tx-sender)
    
    ;; Set card attributes
    (map-set card-power token-id power)
    (map-set card-speed token-id speed)
    (map-set card-defense token-id defense)
    (map-set card-rarity token-id rarity)
    (map-set card-element token-id element)
    (map-set token-uris token-id token-uri)
    
    ;; Update last token ID
    (var-set last-token-id token-id)
    
    ;; Initialize reputation if first card
    (if (is-none (map-get? player-reputation tx-sender))
      (map-set player-reputation tx-sender u100)
      true
    )
    
    (ok token-id)
  )
)

(define-public (transfer (token-id uint) (recipient principal))
  (let (
    (current-owner (unwrap! (map-get? token-owners token-id) err-token-not-found))
  )
    ;; Check if sender is the owner
    (asserts! (is-eq tx-sender current-owner) err-not-token-owner)
    
    ;; Transfer ownership
    (map-set token-owners token-id recipient)
    
    (ok true)
  )
)

(define-public (update-reputation (player principal) (new-reputation uint))
  ;; Only contract owner can update reputation (oracle role)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set player-reputation player new-reputation)
    (ok true)
  )
)

(define-public (set-attribute-multiplier (attribute (string-ascii 20)) (multiplier uint))
  ;; Only contract owner can set multipliers (oracle integration)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set attribute-multipliers attribute multiplier)
    (ok true)
  )
)

(define-public (update-card-power (token-id uint) (new-power uint))
  ;; Only contract owner can update (simulating oracle updates)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? token-owners token-id)) err-token-not-found)
    (map-set card-power token-id new-power)
    (ok true)
  )
)

(define-public (set-mint-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set base-mint-price new-price)
    (ok true)
  )
)

;; Battle simulation function
(define-public (battle (attacker-id uint) (defender-id uint))
  (let (
    (attacker-owner (unwrap! (map-get? token-owners attacker-id) err-token-not-found))
    (defender-owner (unwrap! (map-get? token-owners defender-id) err-token-not-found))
    (attacker-power (unwrap! (calculate-effective-power attacker-id) err-token-not-found))
    (defender-defense (default-to u0 (map-get? card-defense defender-id)))
    (attacker-reputation (default-to u100 (map-get? player-reputation attacker-owner)))
  )
    ;; Check attacker owns their card
    (asserts! (is-eq tx-sender attacker-owner) err-not-token-owner)
    
    ;; Simple battle logic: attacker wins if power > defense
    (if (> attacker-power defender-defense)
      (begin
        ;; Winner gains reputation
        (map-set player-reputation attacker-owner (+ attacker-reputation u10))
        (ok "attacker-wins")
      )
      (ok "defender-wins")
    )
  )
)

;; Initialize default multipliers
(map-set attribute-multipliers "power" u100)
(map-set attribute-multipliers "speed" u100)
(map-set attribute-multipliers "defense" u100)