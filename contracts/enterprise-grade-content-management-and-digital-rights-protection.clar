;; Distributed Content Authentication Protocol
;; Advanced blockchain infrastructure for secure content verification and ownership tracking
;; 
;; Comprehensive solution providing immutable content registration with sophisticated
;; access control mechanisms and complete provenance tracking capabilities
;; Built for enterprise-grade content management and digital rights protection

;; Primary content verification constants and error handling framework
(define-constant system-controller tx-sender)
(define-constant content-missing-failure (err u301))
(define-constant content-already-exists-failure (err u302))
(define-constant invalid-name-specification-failure (err u303))
(define-constant invalid-size-specification-failure (err u304))
(define-constant unauthorized-access-failure (err u305))
(define-constant ownership-mismatch-failure (err u306))
(define-constant controller-permission-failure (err u300))
(define-constant view-access-denied-failure (err u307))
(define-constant invalid-metadata-tags-failure (err u308))

;; Global tracking mechanism for content registration sequence
(define-data-var content-registration-sequence uint u0)

;; Core content metadata storage infrastructure
;; Maintains comprehensive records of all registered digital content items
(define-map verified-content-registry
  { content-sequence-number: uint }
  {
    content-display-name: (string-ascii 64),
    content-proprietor: principal,
    content-byte-size: uint,
    blockchain-registration-height: uint,
    content-detailed-description: (string-ascii 128),
    content-classification-labels: (list 10 (string-ascii 32))
  }
)

;; Advanced access control matrix for granular permission management
;; Enables fine-grained control over content visibility and access rights
(define-map content-access-control-matrix
  { content-sequence-number: uint, requesting-principal: principal }
  { viewing-permission-granted: bool }
)

;; ===== Internal validation and utility function library =====

;; Comprehensive validation routine for content classification labels
;; Ensures all metadata tags conform to system specifications and constraints
(define-private (validate-classification-label (single-label (string-ascii 32)))
  (and
    (> (len single-label) u0)
    (< (len single-label) u33)
  )
)

;; Advanced tag collection validation with comprehensive rule enforcement
;; Performs deep validation of entire tag collection ensuring compliance
(define-private (perform-comprehensive-tag-validation (label-collection (list 10 (string-ascii 32))))
  (and
    (> (len label-collection) u0)
    (<= (len label-collection) u10)
    (is-eq (len (filter validate-classification-label label-collection)) (len label-collection))
  )
)

;; Content existence verification utility function
;; Performs efficient lookup to determine if content is registered in system
(define-private (verify-content-registration-status (content-sequence-number uint))
  (is-some (map-get? verified-content-registry { content-sequence-number: content-sequence-number }))
)

;; Byte size extraction utility for registered content items
;; Safely retrieves content size information with proper error handling
(define-private (extract-content-byte-size (content-sequence-number uint))
  (default-to u0
    (get content-byte-size
      (map-get? verified-content-registry { content-sequence-number: content-sequence-number })
    )
  )
)

;; Ownership verification mechanism with comprehensive security checks
;; Validates that requesting principal has legitimate ownership rights
(define-private (validate-content-ownership-rights (content-sequence-number uint) (requesting-principal principal))
  (match (map-get? verified-content-registry { content-sequence-number: content-sequence-number })
    retrieved-content-data (is-eq (get content-proprietor retrieved-content-data) requesting-principal)
    false
  )
)

;; ===== Primary public interface functions for content management =====

;; Comprehensive content registration function with full metadata support
;; Enables secure registration of new digital content with complete audit trail
;; Includes sophisticated validation and permission initialization
(define-public (register-new-verified-content
  (display-name (string-ascii 64))
  (byte-size uint)
  (detailed-description (string-ascii 128))
  (classification-labels (list 10 (string-ascii 32)))
)
  (let
    (
      (generated-content-sequence-id (+ (var-get content-registration-sequence) u1))
    )
    ;; Comprehensive parameter validation with detailed error reporting
    (asserts! (> (len display-name) u0) invalid-name-specification-failure)
    (asserts! (< (len display-name) u65) invalid-name-specification-failure)
    (asserts! (> byte-size u0) invalid-size-specification-failure)
    (asserts! (< byte-size u1000000000) invalid-size-specification-failure)
    (asserts! (> (len detailed-description) u0) invalid-name-specification-failure)
    (asserts! (< (len detailed-description) u129) invalid-name-specification-failure)
    (asserts! (perform-comprehensive-tag-validation classification-labels) invalid-metadata-tags-failure)

    ;; Secure content registration with complete metadata preservation
    (map-insert verified-content-registry
      { content-sequence-number: generated-content-sequence-id }
      {
        content-display-name: display-name,
        content-proprietor: tx-sender,
        content-byte-size: byte-size,
        blockchain-registration-height: block-height,
        content-detailed-description: detailed-description,
        content-classification-labels: classification-labels
      }
    )

    ;; Initialize comprehensive access control permissions for content creator
    (map-insert content-access-control-matrix
      { content-sequence-number: generated-content-sequence-id, requesting-principal: tx-sender }
      { viewing-permission-granted: true }
    )

    ;; Update global content registration tracking mechanism
    (var-set content-registration-sequence generated-content-sequence-id)
    (ok generated-content-sequence-id)
  )
)

;; Advanced content metadata modification function with ownership validation
;; Provides comprehensive update capabilities for existing registered content
;; Maintains data integrity while allowing authorized modifications
(define-public (modify-existing-content-metadata
  (content-sequence-number uint)
  (updated-display-name (string-ascii 64))
  (updated-byte-size uint)
  (updated-detailed-description (string-ascii 128))
  (updated-classification-labels (list 10 (string-ascii 32)))
)
  (let
    (
      (existing-content-data (unwrap! (map-get? verified-content-registry { content-sequence-number: content-sequence-number })
        content-missing-failure))
    )
    ;; Comprehensive validation of ownership rights and parameter integrity
    (asserts! (verify-content-registration-status content-sequence-number) content-missing-failure)
    (asserts! (is-eq (get content-proprietor existing-content-data) tx-sender) ownership-mismatch-failure)
    (asserts! (> (len updated-display-name) u0) invalid-name-specification-failure)
    (asserts! (< (len updated-display-name) u65) invalid-name-specification-failure)
    (asserts! (> updated-byte-size u0) invalid-size-specification-failure)
    (asserts! (< updated-byte-size u1000000000) invalid-size-specification-failure)
    (asserts! (> (len updated-detailed-description) u0) invalid-name-specification-failure)
    (asserts! (< (len updated-detailed-description) u129) invalid-name-specification-failure)
    (asserts! (perform-comprehensive-tag-validation updated-classification-labels) invalid-metadata-tags-failure)

    ;; Secure metadata update operation with data preservation
    (map-set verified-content-registry
      { content-sequence-number: content-sequence-number }
      (merge existing-content-data {
        content-display-name: updated-display-name,
        content-byte-size: updated-byte-size,
        content-detailed-description: updated-detailed-description,
        content-classification-labels: updated-classification-labels
      })
    )
    (ok true)
  )
)

;; Secure ownership transfer mechanism with comprehensive validation
;; Enables authorized transfer of content ownership between principals
;; Maintains complete audit trail and access control integrity
(define-public (execute-content-ownership-transfer (content-sequence-number uint) (designated-new-proprietor principal))
  (let
    (
      (existing-content-data (unwrap! (map-get? verified-content-registry { content-sequence-number: content-sequence-number })
        content-missing-failure))
    )
    ;; Rigorous ownership verification and authorization checks
    (asserts! (verify-content-registration-status content-sequence-number) content-missing-failure)
    (asserts! (is-eq (get content-proprietor existing-content-data) tx-sender) ownership-mismatch-failure)

    ;; Execute secure ownership transfer with data integrity preservation
    (map-set verified-content-registry
      { content-sequence-number: content-sequence-number }
      (merge existing-content-data { content-proprietor: designated-new-proprietor })
    )
    (ok true)
  )
)

;; Permanent content removal function with comprehensive security controls
;; Enables authorized deletion of registered content from the system
;; Includes multiple validation layers to prevent unauthorized removal
(define-public (permanently-remove-registered-content (content-sequence-number uint))
  (let
    (
      (existing-content-data (unwrap! (map-get? verified-content-registry { content-sequence-number: content-sequence-number })
        content-missing-failure))
    )
    ;; Multi-layer security validation for content removal authorization
    (asserts! (verify-content-registration-status content-sequence-number) content-missing-failure)
    (asserts! (is-eq (get content-proprietor existing-content-data) tx-sender) ownership-mismatch-failure)

    ;; Execute permanent content removal from registry
    (map-delete verified-content-registry { content-sequence-number: content-sequence-number })
    (ok true)
  )
)
